from collections import OrderedDict
from datetime import datetime
import json
import logging
import os
import sys

import boto3


log = logging.getLogger(__name__)


def create_jobs():
    batch_client = boto3.client('batch')

    start_time = datetime.utcnow().strftime('%Y%m%d_%H%M%S')

    first_scenario_id = 5
    number_of_scenarios = 1
    for scenario_id in range(first_scenario_id, first_scenario_id + number_of_scenarios):
        job_parameters = OrderedDict([
            ('SCENARIO_ID', scenario_id),
            ('FIRST_REPETITION_ID', 1),
            ('START_TIME', start_time),
        ])
        number_of_repetitions = 100

        job_params = {
            'jobQueue': os.environ['JOB_QUEUE'],
            'jobDefinition': os.environ['JOB_DEFINITION'],
            'jobName': '-'.join(
                '{}_{}'.format(k, v)
                for k, v in job_parameters.items()
            ),
            'containerOverrides': {
                'environment': [
                    {
                        'name': k,
                        'value': str(v)
                    }
                    for k, v in job_parameters.items()
                ],
            },
        }
        if number_of_repetitions > 1:
            job_params['arrayProperties'] = {
                'size': number_of_repetitions,
            }

        log.info('job_params={}'.format(job_params))
        job_response = batch_client.submit_job(**job_params)
        log.info('job_response={}'.format(job_response))


def handler(event, job_index):
    log.info('main_event={}, job_index={}'.format(event, job_index))

    method_name = event['method']
    method_kwargs = event.get('kwargs') or {}

    globals()[method_name](**method_kwargs)


if __name__ == '__main__':
    logging.basicConfig(style='{', level=os.environ.get('LOG_LEVEL', 'INFO'))

    event_argument = os.environ.get('EVENT')
    if 1 < len(sys.argv):
        if event_argument:
            raise RuntimeError('Ambiguous input: Command-line argument and environment variable EVENT')
        event_argument = sys.argv[1]

    event = event_argument and json.loads(event_argument)
    job_index = int(os.environ.get('AWS_BATCH_JOB_ARRAY_INDEX', '0'))
    handler(event, job_index)
