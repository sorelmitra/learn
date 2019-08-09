from .settings_common import *

# SECURITY WARNING: keep the secret key used in production secret!
SECRET_KEY = 'p9(*o9x_q7pm^89y9sq(y!_wvms(3e)%a$)y6)ys494!dsx)mx'

# SECURITY WARNING: don't run with debug turned on in production!
DEBUG = True

ALLOWED_HOSTS = [
    'localhost'
]

# Database
# https://docs.djangoproject.com/en/2.2/ref/settings/#databases

DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.postgresql',
        'NAME': 'chatapi',
        'USER': 'postgres',
        'HOST': '192.168.99.103',
        'PORT': '5203'
    },
}

