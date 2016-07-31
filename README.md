strava-bot is a Slack bot that tells you about your Strava activities.

### Setup

This project is built with [stack](https://docs.haskellstack.org/en/stable/README/). Clone the repository and run:
```
stack setup
stack build
```

There are also two types of credentials required:
- A webhook URL for [Slack](https://api.slack.com/incoming-webhooks) in `secrets/slack-webhook-url`
- An access token for [Strava](https://strava.github.io/api/) in `secrets/strava-access-token`

### Run

To run the bot:
```
stack exec strava-bot
```
