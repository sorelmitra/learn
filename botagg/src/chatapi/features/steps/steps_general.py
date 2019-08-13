import time

@when(u'Sleep "{secondsStr}" seconds')
def step_impl(context, secondsStr):
	sec = float(secondsStr)
	time.sleep(sec)
