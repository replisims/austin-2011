locals {
  enable_readonly_accounts = length(var.readonly_accounts) > 0

  global_prefix = "annloh-austin-2011"
}


resource "aws_s3_bucket" "this" {
  bucket = "${local.global_prefix}-${var.name}"
  tags   = "${var.tags}"
  force_destroy = true
}


resource "aws_s3_bucket_versioning" "versioning_example" {
  bucket = aws_s3_bucket.this.id
  versioning_configuration {
    status = "Enabled"
  }
}

resource "aws_s3_bucket_lifecycle_configuration" "example" {
  bucket = aws_s3_bucket.this.id

  rule {
    id = "expire_90days"
    expiration {
      days = 90
    }
    status = "Enabled"
  }
}


# data "aws_iam_policy_document" "bucket_policy" {
#   count = "${local.enable_readonly_accounts ? 1 : 0}"

#   statement {
#     sid = "read"
#     resources = [
#       "${aws_s3_bucket.this.arn}",
#       "${aws_s3_bucket.this.arn}/*",
#     ]
#     actions   = [
#       "s3:Get*",
#       "s3:List*",
#     ]
#     principals {
#       type        = "AWS"
#       identifiers = "${var.readonly_accounts}"
#     }
#   }
# }


# resource "aws_s3_bucket_policy" "this" {
#   count  = "${local.enable_readonly_accounts ? 1 : 0}"

#   bucket = aws_s3_bucket.this.id
#   policy = "${data.aws_iam_policy_document.bucket_policy.json}"
# }


# resource "aws_sns_topic" "this" {
#   count        = "${local.enable_sns_topic ? 1 : 0}"

#   name         = "${var.name}-s3"
#   display_name = "MasterThesis"
# }
# locals {
#   sns_topic_name = "${element(concat(aws_sns_topic.this.*.name, list("")), 0)}"
#   sns_topic_arn  = "${element(concat(aws_sns_topic.this.*.arn, list("")), 0)}"
# }

# data "aws_iam_policy_document" "sns_policy_base" {
#   count = "${local.enable_sns_topic ? 1 : 0}"

#   statement {
#     sid       = "publish"
#     resources = ["${local.sns_topic_arn}"]
#     actions   = ["sns:Publish"]
#     principals {
#       type        = "Service"
#       identifiers = ["s3.amazonaws.com"]
#     }
#     condition {
#       test     = "ArnEquals"
#       variable = "aws:SourceArn"
#       values   = ["${local.bucket_arn}"]
#     }
#   }
# }

# data "aws_iam_policy_document" "sns_policy_cross_account" {
#   count = "${local.enable_sns_topic && local.enable_readonly_accounts ? 1 : 0}"

#   statement {
#     sid       = "subscribe"
#     resources = ["${local.sns_topic_arn}"]
#     actions   = [
#       "sns:Subscribe",
#       "sns:GetTopicAttributes",
#       "sns:ListSubscriptionsByTopic",
#     ]
#     principals {
#       type        = "AWS"
#       identifiers = "${var.readonly_accounts}"
#     }
#   }
# }

# data "aws_iam_policy_document" "sns_policy" {
#   count  = "${local.enable_sns_topic ? 1 : 0}"

#   source_json   = "${element(concat(data.aws_iam_policy_document.sns_policy_base.*.json, list("")), 0)}"
#   override_json = "${element(concat(data.aws_iam_policy_document.sns_policy_cross_account.*.json, list("")), 0)}"
#   statement = []
# }

# resource "aws_sns_topic_policy" "this" {
#   count = "${local.enable_sns_topic ? 1 : 0}"
#   arn    = "${local.sns_topic_arn}"
#   policy = "${data.aws_iam_policy_document.sns_policy.json}"
# }


# resource "aws_s3_bucket_notification" "this" {
#   count  = "${local.enable_sns_topic ? 1 : 0}"
#   bucket = "${local.bucket_name}"

#   topic {
#     id        = "${local.sns_topic_name}"
#     topic_arn = "${local.sns_topic_arn}"
#     events    = ["s3:ObjectCreated:*"]
#   }
# }
