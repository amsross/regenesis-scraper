# regenesis-scraper

## local testing

1. start the build server
    ```
    npm run start
    ```

1. start the offline server
    ```
    AWS_ACCOUNT_ID=************ \
      GENESIS_PWORD=****** \
      GENESIS_UNAME=****************** \
      GENESIS_URL=********************************************* \
      npm run local
    ```

1. trigger an event
    ```
    aws lambda invoke /dev/null \
      --endpoint-url http://localhost:3002 \
      --function-name regenesis-scraper-***-grades_write \
      --invocation-type Event \
      --cli-binary-format  raw-in-base64-out \
      --payload '{"studentid":*******}'
    ```
