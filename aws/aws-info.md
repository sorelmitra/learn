# Checking and Troubleshooting CLI Access

To check if you have access into AWS from the CLI:

	AWS_PROFILE=my-profile aws sts get-caller-identity

Output:

```json
{
    "UserId": "<ID1>:<email>",
    "Account": "<account-number>",
    "Arn": "arn:aws:sts::<account-number>:assumed-role/AWSReservedSSO_<role-name>_<ID2>/<email>"
}
```

**Troubleshoot**

If you get an error like this:

```
An error occurred (InvalidClientTokenId) when calling 
the GetCallerIdentity operation: 
The security token included in the request is invalid
```

Then delete `~/.aws/cli` and `~/.aws/sso`...

---
---
---
---
---
---
---
---
---

# Security

## IP whitelisting

Add your IP to <AWS account> -> (AWS Item, e.g. RDS -> Databases -> <name>) -> VPC security groups -> first group -> Inbound rules -> Add -> MSSQL, my-IP/32 -> Save.

---
---
---
---
---
---
---
---
---

# Profiles

## Local Test Profile

	aws configure set aws_access_key_id "dummy" --profile test-profile && aws configure set aws_secret_access_key "dummy" --profile test-profile && aws configure set region "us-west-2" --profile test-profile

---
---
---
---
---
---
---
---
---

# Local Sessions

**Step 1**: Install the Session Manager Plugin for the AWS CLI.

**Step 2**: Start the local DB connection relay server:

	AWS_PROFILE="<AwsProfile>" aws ssm start-session \
	    --region "<Region>" \
	    --target "<ManagedNode>" \
	    --document-name AWS-StartPortForwardingSessionToRemoteHost \
	    --parameters host="<Host>",portNumber="<PortNumber>",localPortNumber="<LocalPortNumber>"

Where:

- AwsProfile := The name of the AWS profile as defined in `~/.aws/config`.
- Region := The AWS region that you're using.
- ManagedNode := The host configured in AWS for remote access.  Typically it is an EC2 instance that has connectivity to the resources you need access to from your local machine (e.g. a database server).
- Host := The name of the resource you need access to, e.g. the RDS cluster instance name.
- PortNumber := The remote port.
- LocalPortNumber := The local port.

**Step 4**: Make sure to keep the relay server running for as long as you need access to the cloud resources.

**Step 5**: Connect your software (e.g. DB admin) to `localhost:<LocalPortNumber>`.

---
---
---
---
---
---
---
---
---

# Elastic Container Service

## See Environment Variables

Elastic Container Service -> Task Definitions -> filter by name -> choose your service -> choose your revision -> Create new revision (although it sounds weird) -> scroll down -> choose your container -> scroll down -> Environment variables

After taking note of the values you need, click Cancel in the Edit container panel and click Cancel again in the Create new revision of Task Definition page.

---
---
---
---
---
---
---
---
---

# CloudWatch

## Count invocations in a given period

	aws cloudwatch get-metric-statistics --namespace AWS/Lambda --metric-name Invocations --start-time 2023-07-18T08:52Z --end-time 2023-07-18T10:03Z --period 3600 --statistics Sum --dimensions Name=FunctionName,Value=<func-name> --profile <aws-profile>

---
---
---
---
---
---
---
---
---

# IAM

## List all Access Keys

	aws iam list-users | grep '"UserName"' | awk '{print $2}' | sed -e 's/,//g' | xargs -L 1 -J % aws iam list-access-keys --user-name %

## Forcing MFA

https://forums.aws.amazon.com/thread.jspa?threadID=154971

---
---
---
---
---
---
---
---
---

# Secrets Manager

It supports versioning.  To see versions of a secret:

	aws secretsmanager list-secret-version-ids --secret-id <secret-id> --include-deprecated --profile devint01-profile

To see the value of a particular version:

	aws secretsmanager get-secret-value --secret-id <secret-id> --version-id <version-id> --profile devint01-profile
