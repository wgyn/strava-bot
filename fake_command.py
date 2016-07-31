#!/usr/bin/env python

import requests

data = {
    'user_name': 'ryan',
    'channel_id': '1',
    'channel_name': 'general',
    'command': '/strava',
    'text': 'hello, world!',
}

# Fakes a command from Slack
requests.post('http://localhost:8000', data=data)
