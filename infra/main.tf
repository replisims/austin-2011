terraform {
  required_version = "1.2.0"

  backend "s3" {
    region = "eu-west-1"
  }

  required_providers {
    aws = {
      source = "aws"
      version = "4.15.1"
    }
    random = {
      source = "random"
      version = "3.2.0"
    }
  }
}


locals {
  region = "eu-west-1"

  product = "austin-2011"

  name_prefix = "${local.product}-${var.environment}"
  global_prefix = "annloh-${local.product}-${var.environment}"

  trusted_readonly_accounts = []

  tags = {
    Product     = "${local.product}"
    Environment = "${var.environment}"
    Terraform   = "github.com:replisims/austin-2011 s3://annloh-terraform-state/${local.product}-${var.environment}.tfstate"
  }

  # default_az = "eu-west-1b"
}


provider "aws" {
  region = local.region
}
provider "random" {}


module "vpc" {
  source  = "terraform-aws-modules/vpc/aws"
  version = "3.14.0"

  name = "${local.name_prefix}"
  tags = "${local.tags}"

  azs              = ["${local.region}a", "${local.region}b", "${local.region}c"]
  cidr             = "10.89.0.0/16"
  public_subnets   = ["10.89.0.0/20", "10.89.16.0/20", "10.89.32.0/20"]

  enable_dns_hostnames = true
}


module "ecr_repo" {
  source = "./modules/ecr-repo"
  name   = "${local.name_prefix}"
}

module "s3_bucket_cache" {
  source  = "./modules/s3-bucket"
  name    = "${var.environment}-cache"
  region  = "${local.region}"
  tags    = "${local.tags}"
  # hardcoded within the module now
  # lifecycle_rules = [
  #   {
  #     enabled = true
  #     expiration = [{
  #       days = 90
  #     }]
  #   },
  # ]
}


data "aws_iam_policy_document" "batch_job" {
  statement {
    resources = [
      "${module.s3_bucket_cache.arn}",
      "${module.s3_bucket_cache.arn}/*",
    ]
    actions   = ["s3:*"]
  }

  statement {
    resources = ["*"]
    actions   = ["s3:Get*", "s3:List*"]
  }
}

module "batch" {
  source                = "./modules/batch"
  name_prefix           = "${local.name_prefix}"
  tags                  = "${local.tags}"
  job_vcpus_large       = "${var.worker_job_vcpus_large}"
  job_memory_large      = "${var.worker_job_memory_large}"
  worker_instance_type  = "${var.worker_instance_type}"
  min_vcpus             = "${var.min_vcpus}"
  max_vcpus             = "${var.max_vcpus}"
  job_policy_document   = "${data.aws_iam_policy_document.batch_job.json}"

  environment_variables = [
    {
      name  = "CACHE_DIR",
      value = "/tmp/cache",
    },
    {
      name  = "RESULTS_BUCKET",
      value = "${module.s3_bucket_cache.id}",
    },
    {
      name  = "RESULTS_DIR",
      value = "batch/v5",
    },
  ]

  subnets        = module.vpc.public_subnets
  security_group = module.vpc.default_security_group_id

  repository_url = module.ecr_repo.repository_url
}
