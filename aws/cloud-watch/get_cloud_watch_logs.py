#!/usr/bin/env python
import sys
import os
import time
import math
import calendar
import threading
import argparse
import boto3
from functools import partial

def split(l, n):
  size = int(math.ceil(len(l)/float(n)))
  cutoff = len(l) % n
  result = []
  pos = 0
  for i in range(0, n):
    end = pos + size if cutoff == 0 or i < cutoff else pos + size - 1
    result.append(l[pos:end])
    pos = end
  return result

def get_streams(group, start, end, prefix):
  client = boto3.client('logs')
  token = None
  first = True
  streams = []
  if prefix:
    get_streams = partial(client.describe_log_streams, logGroupName=group, logStreamNamePrefix=prefix)
  else:
    get_streams = partial(client.describe_log_streams, logGroupName=group)
  while first or token:
    if token:
      batch = get_streams(nextToken=token)
    else:
      batch = get_streams()
    if not batch['logStreams']:
      break
    for s in batch['logStreams']:
      if s.get('lastEventTimestamp', start - 1) >= start or s.get('firstEventTimestamp', end + 1) <= end:
        streams.append(s['logStreamName'])
    token = batch.get('nextToken')
    first = False
  return streams

def stream(group, streams, start, end):
  client = boto3.client('logs')
  # client.meta.events._unique_id_handlers['retry-config-logs']['handler']._checker.__dict__['_max_attempts'] = 10
  for s in streams:
    sys.stdout.flush()
    token = None
    first = True
    while first or token:
      if token:
        events = client.get_log_events(logGroupName=group, logStreamName=s, nextToken=token)
      else:
        events = client.get_log_events(logGroupName=group, logStreamName=s)
      if not events['events']:
        break
      for e in events['events']:
         if e['timestamp'] >= start and e['timestamp'] <= end:
           sys.stdout.write(e['message'] + os.linesep)
      token = events.get('nextForwardToken')
      first = False

if __name__ == "__main__":
  #build args
  parser = argparse.ArgumentParser()
  parser.add_argument('--log-group', type=str, help='The name of the log group whose logs we are targeting', required=True)
  parser.add_argument('--start-epoch-milli', type=int, help='Show only stuff that is later than or equal to this', default=0)
  parser.add_argument('--end-epoch-milli', type=int, help='Show only stuff that is earlier than or equal to this', default=calendar.timegm(time.gmtime()) * 1000)
  parser.add_argument('--stream-name-prefix', type=str, help='A prefix to only show log streams whose names match')
  parser.add_argument('--threads', type=int, help='How many threads to use at one time', default=1)
  args = parser.parse_args()

  stage=os.environ['STAGE']
  boto3.setup_default_session(profile_name=f'{stage}-profile')
  #get the proper log streams
  streams = get_streams(args.log_group, args.start_epoch_milli, args.end_epoch_milli, args.stream_name_prefix)

  #for each chunk of streams spawn a thread to get them
  streams = split(streams, args.threads)
  threads = []
  for s in streams:
    threads.append(threading.Thread(target=stream, args=(args.log_group, s, args.start_epoch_milli, args.end_epoch_milli)))
    threads[-1].start()
  for t in threads:
    t.join()
