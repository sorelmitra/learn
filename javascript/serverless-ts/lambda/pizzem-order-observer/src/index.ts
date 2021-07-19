import SNS from 'aws-sdk/clients/sns';

export const handler = async (event, context) => {
    console.log(event.Records);
};
