# regenesis-scraper

## local testing

1. start the build server

   ```
   npm run start
   ```

1. create some env variables

```env
AWS_ACCOUNT_ID=************
GENESIS_PWORD=******
GENESIS_UNAME=******************
GENESIS_URL=*********************************************
```

1. start the offline server

   ```
   AWS_ACCOUNT_ID=$AWS_ACCOUNT_ID \
     GENESIS_PWORD=$GENESIS_PWORD \
     GENESIS_UNAME=$GENESIS_UNAME \
     GENESIS_URL=$GENESIS_URL \
     npm run local
   ```

1. trigger an event

   ```
   AWS_ACCOUNT_ID=$AWS_ACCOUNT_ID \
     GENESIS_PWORD=$GENESIS_PWORD \
     GENESIS_UNAME=$GENESIS_UNAME \
     GENESIS_URL=$GENESIS_URL \
     npx serverless invoke local \
     --function grades_write \
     --data '{"studentid":*******}'
   ```
