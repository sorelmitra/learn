#!/bin/zsh

# set -o xtrace

SCRIPT_NAME=$(basename -- "$0")

DO_DELETE=0
DO_SHOW_SIZE=0

usage() {
	echo
	echo "Usage: ${SCRIPT_NAME} [-d]"
	echo
	echo "Recursively cleanup build files for various languages.  Finds (and optionally deletes) hardcoded build directories or files in the current directory."
	echo
	echo "Parameters:"
	echo "	-s: Optional, if present it also showws the size of the build things."
	echo "	-d: Optional, if present it also deletes build things."
	echo
}

add_to_find_command() {
	# Exclude subdirectories in a directory of the same name,
	# i.e. node_modules/a/.../z/node_modules, which is common with JS
	EXCLUDE_SUBDIR_OF_SAME_NAME=""
	if [[ "${FILE_TYPE}" = "d" ]]; then
		for PARENT_BUILD_DIR in ${BUILD_DIRECTORIES[@]}; do
			EXCLUDE_SUBDIR_OF_SAME_NAME="${EXCLUDE_SUBDIR_OF_SAME_NAME} -not -path '*/${PARENT_BUILD_DIR}/*'"
		done
	fi

	if (( ${IS_FIRST_ITEM} == 1 )); then
		IS_FIRST_ITEM=0
	else
		FIND_COMMAND="${FIND_COMMAND} -o"
	fi
	FIND_COMMAND="${FIND_COMMAND} -type ${FILE_TYPE} -name '${BUILD_ITEM}' ${EXCLUDE_SUBDIR_OF_SAME_NAME}"
}

while getopts 'ds' OPTION; do
	case $OPTION in
	d)
		DO_DELETE=1
		;;
	s)
		DO_SHOW_SIZE=1
		;;
	?)
		usage
		exit 2
		;;
	esac
done

BUILD_FILES=("")
BUILD_DIRECTORIES=("node_modules" "target" "__pycache__" "pyvirtenv")
# For "pyvirtenv" see github/sorelmitra/learn/botagg/chatapi - it's a conventional
# name I use for virtualenv for Python

FIND_COMMAND="find ."
IS_FIRST_ITEM=1

for BUILD_ITEM in ${BUILD_FILES[@]}; do
	FILE_TYPE="f"
	add_to_find_command
done

for BUILD_ITEM in ${BUILD_DIRECTORIES[@]}; do
	FILE_TYPE="d"
	add_to_find_command
done

echo ${FIND_COMMAND}
echo
echo "Hold on while we find your things..."
RESULT=`eval ${FIND_COMMAND}`

JUST_SHOW=1

if (( ${DO_SHOW_SIZE} == 1 )); then
	JUST_SHOW=0
	echo
	echo "Here's the size of your things:"
	echo ${RESULT} | xargs du -hs -c
fi

# Order DOES matter!  First show and then delete!

if (( ${DO_DELETE} == 1 )); then
	JUST_SHOW=0
	echo
	echo -n "Here we REMOVE your things.  Are you sure? Y/[N] > "
	read USER_ANSWER
	if [[ "${USER_ANSWER}" = "Y" ]]; then
		echo
		echo "Removing your things..."
		echo ${RESULT} | xargs -n 1 rm -rf
		echo "Done."
	fi
fi

if (( ${JUST_SHOW} == 1 )); then
	echo
	echo "Here are your things:"
	echo ${RESULT}
fi

