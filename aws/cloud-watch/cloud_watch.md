Determine the AWS profile you're gonna use, say `dev-profile`.

Make sure you're logged in to AWS CLI to that profile.

Head to [currentmillis](https://currentmillis.com).

Use it to find millis-since-epoch for the start or end date for your logs.  Say you end up with `1670924647960`.

Run this command:

	STAGE=dev /Users/sorel/Downloads/get_cloud_watch_logs.py --log-group my-log --threads 10 --start-epoch-milli 1670924647960

This will go to your AWS profile (notice that `-profile` is added by the script), start 10 threads that will fetch logs from ALL streams within the log group `my-log`, and print them to your terminal.

**Note**: The script is slow to download logs, even with threads.  This is due to the latency to the AWS servers and APIs.
