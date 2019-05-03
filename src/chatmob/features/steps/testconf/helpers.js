
function getAppPath(isAndroid) {
  if (process.env.BITRISE) {
    return process.env.BITRISE_APK_PATH
  } else if (isAndroid) {
    return `${process.cwd()}/android/app/build/outputs/apk/app-debug.apk`
  } else {
    return `${process.cwd()}/bin/Exponent.app`
  }

}

exports.helpers = {
  getAppPath,
}