import { Function, StackContext } from 'sst/constructs';

export function AuthStack({ stack }: StackContext) {
    const authorizer = new Function(stack, 'authorizer-fn', {
        handler: 'DotNetSstLambda::DotNetSstLambda.Authorizer::Authorize',
    });

    return { authorizer };
}
