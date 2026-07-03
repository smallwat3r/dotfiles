# Android SDK
#
# Adds platform-tools (adb, fastboot) to PATH.

export ANDROID_HOME="$HOME/Android/Sdk"
path_add "$ANDROID_HOME/platform-tools"
