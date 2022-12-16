data "aws_iam_policy_document" "instance_assume_role" {
  statement {
    actions = ["sts:AssumeRole"]

    principals {
      type        = "Service"
      identifiers = ["ec2.amazonaws.com"]
    }
  }
}

resource "aws_iam_role" "batch_instance" {
  assume_role_policy = "${data.aws_iam_policy_document.instance_assume_role.json}"
  name               = "${var.name_prefix}-batch-instance"
}

resource "aws_iam_role_policy_attachment" "ecs_instance_role" {
  role       = "${aws_iam_role.batch_instance.name}"
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonEC2ContainerServiceforEC2Role"
}

resource "aws_iam_instance_profile" "batch_instance" {
  name = "${var.name_prefix}-batch-instance"
  role = "${aws_iam_role.batch_instance.name}"
}

data "aws_iam_policy_document" "batch_service" {
  statement {
    actions = ["sts:AssumeRole"]

    principals {
      type        = "Service"
      identifiers = ["batch.amazonaws.com"]
    }
  }
}

resource "aws_iam_role" "batch_service" {
  assume_role_policy = "${data.aws_iam_policy_document.batch_service.json}"
  name               = "${var.name_prefix}-batch-service"
}

resource "aws_iam_role_policy_attachment" "batch_service" {
  role       = "${aws_iam_role.batch_service.name}"
  policy_arn = "arn:aws:iam::aws:policy/service-role/AWSBatchServiceRole"
}

data "aws_iam_policy_document" "monitoring_policy" {
  statement {
    actions = [
      "cloudwatch:*",
      "logs:*",
    ]

    resources = ["*"]
  }
}

resource "aws_iam_role_policy" "monitoring_policy" {
  name   = "monitoring"
  policy = "${data.aws_iam_policy_document.monitoring_policy.json}"
  role   = "${aws_iam_role.batch_instance.id}"
}

data "aws_iam_policy_document" "tagging_policy" {
  statement {
    actions   = ["ec2:CreateTags"]
    resources = ["*"]
  }
}

resource "aws_iam_role_policy" "tagging_policy" {
  name   = "tagging"
  policy = "${data.aws_iam_policy_document.tagging_policy.json}"
  role   = "${aws_iam_role.batch_instance.id}"
}

data "aws_iam_policy_document" "ecs_policy" {
  statement {
    actions = [
      "ecs:Submit*",
      "ecs:StartTelemetrySession",
      "ecs:StartTask",
      "ecs:RegisterContainerInstance",
      "ecs:Poll",
      "ecs:DiscoverPollEndpoint",
      "ecs:DeregisterContainerInstance",
      "ecs:CreateCluster",
    ]

    resources = ["*"]
  }
}

resource "aws_iam_role_policy" "ecs_policy" {
  name   = "ecs"
  policy = "${data.aws_iam_policy_document.ecs_policy.json}"
  role   = "${aws_iam_role.batch_instance.id}"
}


resource "random_id" "compute_environment_name_suffix" {
  byte_length = 4

  keepers = {
    instance_type  = var.worker_instance_type
    subnet         = join(",", var.subnets)
    security_group = var.security_group
    version        = 2
  }
}
resource "aws_batch_compute_environment" "this" {
  compute_environment_name = "${var.name_prefix}-${random_id.compute_environment_name_suffix.hex}"

  compute_resources {
    instance_role = "${aws_iam_instance_profile.batch_instance.arn}"

    instance_type = [
      "${var.worker_instance_type}",
    ]

    max_vcpus = "${var.max_vcpus}"
    min_vcpus = "${var.min_vcpus}"
    # Needed because of a bug in terraform that will always submit a desired_vcpus to the
    # AWS API call although in theory the parameter is optional.
    # See https://github.com/terraform-providers/terraform-provider-aws/pull/4855
    # WARNING: This workaround leads to a temporary scale-out of the compute environment up to max_vcpus!
    # TODO PL 2019-02-28 Remove this when terraform is fixed
    # desired_vcpus = "${var.max_vcpus}"

    security_group_ids = ["${var.security_group}"]

    subnets = var.subnets
    type    = "SPOT"
    bid_percentage = 99

    spot_iam_fleet_role = "arn:aws:iam::449601346066:role/AmazonEC2SpotFleetRole"

    tags = "${var.tags}"
  }

  service_role = "${aws_iam_role.batch_service.arn}"
  type         = "MANAGED"

  depends_on = [
    aws_iam_role_policy_attachment.batch_service,
    aws_iam_role_policy_attachment.ecs_instance_role,
  ]

  lifecycle {
    create_before_destroy = true
    ignore_changes = [compute_resources[0].desired_vcpus]
  }
}

data "aws_iam_policy_document" "job_assume_role" {
  statement {
    actions = ["sts:AssumeRole"]

    principals {
      type        = "Service"
      identifiers = ["ecs-tasks.amazonaws.com"]
    }
  }
}

resource "aws_iam_role" "job" {
  assume_role_policy = "${data.aws_iam_policy_document.job_assume_role.json}"
  name               = "${var.name_prefix}-batch-job"
}

resource "aws_iam_role_policy" "job_main" {
  name   = "main"
  policy = "${var.job_policy_document}"
  role   = "${aws_iam_role.job.id}"
}

locals {
  container_properties_large = {
    command = ["cloud/wrap_cloud.sh"],
    image = "${var.repository_url}:latest",
    jobRoleArn = "${aws_iam_role.job.arn}",
    vcpus = "${var.job_vcpus_large}",
    memory = "${var.job_memory_large}",
    environment = "${var.environment_variables}",
    volumes = [
      {
        host = {
          sourcePath = "/tmp"
        },
        name = "tmp"
      }
    ],
    mountPoints = [
      {
        sourceVolume = "tmp",
        containerPath = "/tmp"
      }
    ]
  }
}

resource "aws_batch_job_definition" "large" {
  name = "${var.name_prefix}-large"
  type = "container"

  # As per https://aws.amazon.com/blogs/hpc/aws-batch-best-practices/
  retry_strategy {
    attempts = 5

    evaluate_on_exit {
      action = "RETRY"
      on_status_reason = "Host EC2*"
    }

    evaluate_on_exit {
      action = "EXIT"
      on_reason = "*"
    }
  }


  timeout {
    attempt_duration_seconds = 172800  # 48 hours
  }

  # replace(replace(...)) as workaround for terraform bug,
  # as per https://github.com/hashicorp/terraform/issues/17033#issuecomment-399908596
  container_properties = "${replace(replace(jsonencode(local.container_properties_large),
                                            "/\"([0-9]+\\.?[0-9]*)\"/",
                                            "$1"),
                                    "string:",
                                    "")}"
}


resource "aws_batch_job_queue" "main_queue" {
  name     = "${var.name_prefix}"
  state    = "ENABLED"
  priority = 1

  compute_environments = ["${aws_batch_compute_environment.this.arn}"]
}
