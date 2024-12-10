import datetime
import jwt


def get_token_expiration_date(decoded_token):
	"""
    Retrieves the expiration date from a decoded JWT token.

    Parameters:
        decoded_token (dict): The decoded JWT token.

    Returns:
        datetime.datetime: The expiration date of the token.

    Raises:
        AssertionError: If the 'exp' claim is not found in the decoded token.

    Note:
        This function assumes that the 'exp' claim in the JWT is stored as an ISO 8601 string.
        It will raise an AssertionError if the 'exp' claim is missing.
    """
	token_expiry_str = decoded_token.get('exp', None)
	assert token_expiry_str is not None, "Token expiration claim ('exp') is missing."
	token_expiry_date = datetime.datetime.fromisoformat(token_expiry_str)
	return token_expiry_date


def decode_token(access_token):
	"""
    Decodes a JWT token without verifying its signature.

    Parameters:
        access_token (str): The JWT token to decode.

    Returns:
        dict: The decoded token as a dictionary.

    Note:
        This function explicitly disables signature verification by setting "verify_signature" to False
        in the options. This is useful for quickly decoding a token's payload without needing the secret
        or public key. However, this should not be used in production for verifying the authenticity of a
        token, as it would not ensure the token's integrity or authenticity.
    """
	decoded_token = jwt.decode(access_token, algorithms=["RS256", "HS256"],
							   options={"verify_signature": False})
	return decoded_token
