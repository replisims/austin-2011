output "definition_large_arn" {
  value = "${aws_batch_job_definition.large.arn}"
}

output "queue_arn" {
  value = "${aws_batch_job_queue.main_queue.arn}"
}

output "iam_role_batch_instance_arn" {
  value = "${aws_iam_role.batch_instance.arn}"
}
