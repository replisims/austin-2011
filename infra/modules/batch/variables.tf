variable "name_prefix" {}

variable "tags" {
  type = map
}

variable "job_vcpus_large" {}
variable "job_memory_large" {}

variable "worker_instance_type" {}

variable "min_vcpus" {}
variable "max_vcpus" {}

variable "subnets" {
  type = list
}
variable "security_group" {}

variable "job_policy_document" {}

variable "repository_url" {}

variable "environment_variables" {
  type = list
}
