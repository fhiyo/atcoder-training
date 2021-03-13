#!/usr/bin/env bash
# Author: fhiyo

set -u

readonly LANGS=(clisp haskell py3 cpp17 java)
readonly LANG_EXT=(.lisp .hs .py .cpp .java)

readonly PYPATH=util/python
readonly VENV_BIN=.venv/bin
readonly BOOST_PATH=/usr/local/Cellar/boost/1.75.0_2

usage() {
  echo "Usage: $0 [LANG] OPTIONS [PROBLEM NUMBER]

  LANG:
    ${LANGS[0]}
    ${LANGS[1]}
    ${LANGS[2]}
    ${LANGS[3]}
    ${LANGS[4]}

  OPTIONS:
    -h, --help                                                      Print usage (LANG not needed)
    --open                          [PROBLEM NUMBER]                Open web page (LANG not needed)
    -m, --make-env                  [URL]                           Create need directory and file (LANG not needed)

    -c, --clean                                                     Remove files made when build
    -e, --edit                      [PROBLEM NUMBER]                Edit source file
    -g, --git-add                   [PROBLEM NUMBER]                Staging [PROBLEM NUMBER] to git
    -l, --lint                      [PROBLEM NUMBER]                Check haskell coding style (hlint using)
    -r, --run                       [PROBLEM NUMBER]                Run haskell program (no input files)
    -a, --all-test                  [PROBLEM NUMBER]                Test the program is green or red
    -t, --test                      [PROBLEM NUMBER] [TEST NUMBER]  Only run particular test program (specified by [TEST NUMBER])
    -i, --add-input                 [PROBLEM NUMBER]                Add input text file
    -o, --add-output                [PROBLEM NUMBER]                Add output text file
    --copy                          [PROBLEM NUMBER]                Copy code
    --cc, --copy_code_and_open_page [PROBLEM NUMBER]                Copy code and open problem page
  "
}

containsElement () {
  # ref: https://stackoverflow.com/questions/3685970/check-if-a-bash-array-contains-a-value
  declare -r match="$1"
  shift
  for e in "$@"; do
    [[ "${e}" == "${match}" ]] && return 0;
  done
  return 1
}

isExist() {
  if [ $# != 1 ]; then
    echo "Usage: $0 <file or dir path>" 1>&2
    exit 1
  fi
  local path_=$1
  if [ ! -e ${path_} ]; then
    echo "${path_}: No such file or directory" 1>&2
    exit 1
  fi
}

sourcePath() {
  declare -r L=$1
  declare -r PROBLEM=$2

  local iter=0
  for lang_ in "${LANGS[@]}"; do
    if [ ${L} == ${lang_} ]; then
      if [ ${L} == "java" ]; then
        declare -r SOURCE="src/${L}/${PROBLEM}/Main${LANG_EXT[${iter}]}"
      else
        declare -r SOURCE="src/${L}/${PROBLEM}/${PROBLEM}${LANG_EXT[${iter}]}"
      fi
      break
    fi
    (( iter++ ))
  done

  if [ ${iter} -ge ${#LANGS[@]} ]; then
    echo "LANG must be one of the following: \"${LANGS[@]}\"" 1>&2
    exit 1
  fi

  echo ${SOURCE}
}

generate-new-test-number() {
  if [ $# != 2 ]; then
    echo "Usage: $0 <problem_number> <input/output>" 1>&2
    exit 1
  fi

  declare -r PROBLEM_=$1
  declare -r DIR="test/${PROBLEM_}/$2"

  isExist ${DIR}

  if [[ -z $(ls ${DIR}) ]]; then
    new_test_num=1
  else
    new_test_num=$(ls ${DIR} | sort -n | tail -1 | xargs -I{} basename {} .txt)
    (( new_test_num++ ))
  fi
}

addInput() {
  if [ $# != 1 ]; then
    echo "Usage: $0 <problem_number>" 1>&2
    exit 1
  fi

  declare -r PROBLEM=$1
  declare -r INPUT="test/${PROBLEM}/input"

  isExist ${INPUT}
  generate-new-test-number ${PROBLEM} "input"
  vim ${INPUT}/${new_test_num}.txt
}

addOutput() {
  if [ $# != 1 ]; then
    echo "Usage: $0 <problem_number>" 1>&2
    exit 1
  fi

  declare -r PROBLEM=$1
  declare -r OUTPUT="test/${PROBLEM}/output"

  isExist ${OUTPUT}
  generate-new-test-number ${PROBLEM} "output"
  vim ${OUTPUT}/${new_test_num}.txt
}

edit() {
  if [ $# != 2 ]; then
    echo "Usage: $0 <LANG> <problem_number>" 1>&2
    exit 1
  fi

  declare -r L=$1
  declare -r PROBLEM=$2
  declare -r SOURCE=$(sourcePath ${L} ${PROBLEM})
  declare -r DIR=$(dirname ${SOURCE})

  code -n *.code-workspace ${SOURCE}
}

run() {
  if [ $# != 2 ]; then
    echo "Usage: $0 <LANG> <problem_number>" 1>&2
    exit 1
  fi

  declare -r L=$1
  declare -r PROBLEM=$2

  declare -r SOURCE=$(sourcePath ${L} ${PROBLEM})

  if [ ${L} == ${LANGS[0]} ]; then
    # Use sbcl
    sbcl --script ${SOURCE}
  elif [ ${L} == ${LANGS[1]} ]; then
    stack ghc ${SOURCE} > /dev/null
    if [[ $? -ne 0 ]]; then
      echo "ghc comlile is failed..." >&2
      exit 1
    fi
    ./${SOURCE/.hs/}
  elif [ ${L} == ${LANGS[2]} ]; then
    ${PYPATH}/${VENV_BIN}/python ${SOURCE}
  elif [ ${L} == ${LANGS[3]} ]; then
    if [ -f ${SOURCE}/a.out ]; then
      rm ${SOURCE}/a.out
    fi
    g++-9 -std=gnu++17 -Wall -Wextra -O2 -DONLINE_JUDGE -I${BOOST_PATH}/include -L${BOOST_PATH}/lib -o $(dirname ${SOURCE})/a.out ${SOURCE}
    # XXX: grepの仕様で、matchしない場合のexit statusが1になるのでそこを吸収する
    if [[ $? -ne 0 && $? -ne 1 ]]; then
      echo "g++ comlile is failed..." >&2
      exit 1
    fi
    $(dirname ${SOURCE})/a.out
  elif [ ${L} == ${LANGS[4]} ]; then
    javac ${SOURCE}
    java -classpath $(dirname ${SOURCE}) Main
  else
    echo "LANG must be one of the following: ${LANGS}" 1>&2
    exit 1
  fi
}

addToGit() {
  if [ $# != 2 ]; then
    echo "Usage: $0 <LANG> <problem_number>" 1>&2
    exit 1
  fi

  declare -r L=$1
  declare -r PROBLEM=$2
  declare -r INPUT="test/${PROBLEM}/input"
  declare -r OUTPUT="test/${PROBLEM}/output"

  declare -r SOURCE=$(sourcePath ${L} ${PROBLEM})

  isExist ${SOURCE}
  isExist ${INPUT}
  isExist ${OUTPUT}

  git add ${SOURCE}

  if [[ -z $(ls ${INPUT}) ]]; then
    echo -e "\nNo input files.\n"
  else
    git add ${INPUT}
  fi
  if [[ -z $(ls ${OUTPUT}) ]]; then
    echo -e "\nNo input files.\n"
  else
    git add ${OUTPUT}
  fi
}

test_() {
  if [ $# != 3 ]; then
    echo "Usage: $0 <LANG> <problem_number> <test_number>" 1>&2
    exit 1
  fi

  declare -r L=$1
  declare -r PROBLEM=$2
  declare -r TEST_NUM=$3

  declare -r INPUT="test/${PROBLEM}/input/${TEST_NUM}.txt"
  declare -r OUTPUT="test/${PROBLEM}/output/${TEST_NUM}.txt"

  declare -r SOURCE=$(sourcePath ${L} ${PROBLEM})

  isExist ${SOURCE}
  isExist ${INPUT}
  isExist ${OUTPUT}

  cat_command_str="===== cat ${INPUT} ====="
  echo -e "\n${cat_command_str}"
  cat ${INPUT}
  # XXX: This is NOT work: "printf '%.s=' {1..${#cat_command_str}}"
  # ref: https://unix.stackexchange.com/questions/7738/how-can-i-use-variable-in-a-shell-brace-expansion-of-a-sequence
  printf '%.s=' $(seq 1 ${#cat_command_str})
  echo -e "\n"
  diff <(cat ${INPUT} | run ${L} ${PROBLEM}) <(cat ${OUTPUT})
  if [ $? != 0 ]; then
    echo -e "test case: $(basename ${INPUT})  --  Condition RED...\n" 1>&2
  else
    echo -e "test case: $(basename ${INPUT})  --  Condition GREEN.\n"
  fi

  if [[ -z $(ls ${INPUT}) ]]; then
    echo -e "\nNot found the test file: ${INPUT}\n"
  else
    echo -e "\nTest end up.\n"
  fi
}

allTest() {
  if [ $# != 2 ]; then
    echo "Usage: $0 <LANG> <problem_number>" 1>&2
    exit 1
  fi

  declare -r L=$1
  declare -r PROBLEM=$2

  declare -r INPUT="test/${PROBLEM}/input"
  declare -r OUTPUT="test/${PROBLEM}/output"

  declare -r SOURCE=$(sourcePath ${L} ${PROBLEM})

  isExist ${SOURCE}
  isExist ${INPUT}
  isExist ${OUTPUT}

  for test_case in $(ls ${INPUT}); do
    diff <(cat ${INPUT}/${test_case} | run ${L} ${PROBLEM}) <(cat ${OUTPUT}/${test_case})
    if [ $? != 0 ]; then
      echo -e "test case: ${test_case}  --  Condition RED...\n" 1>&2
    else
      echo -e "test case: ${test_case}  --  Condition GREEN.\n"
    fi
  done

  if [[ -z $(ls ${INPUT}) ]]; then
    echo -e "\nNo test files.\n"
  else
    echo -e "\nTest end up.\n"
  fi
}

copyCode() {
  if [ $# != 2 ]; then
    echo "Usage: $0 <LANG> <problem_number> <test_number>" 1>&2
    exit 1
  fi

  declare -r L=$1
  declare -r PROBLEM=$2

  ${PYPATH}/${VENV_BIN}/python ${PYPATH}/copy_code_and_open_page.py --problem_number ${PROBLEM} --lang "${L}"
}


copyCodeAndOpenPage() {
  if [ $# != 2 ]; then
    echo "Usage: $0 <LANG> <problem_number> <test_number>" 1>&2
    exit 1
  fi

  declare -r L=$1
  declare -r PROBLEM=$2

  ${PYPATH}/${VENV_BIN}/python ${PYPATH}/copy_code_and_open_page.py --problem_number ${PROBLEM} --lang "${L}" --open
}

makeEnv() {
  if [ $# != 1 ]; then
    echo "Usage: $0 <URL>" 1>&2
    exit 1
  fi

  declare -r URL=$1

  ${PYPATH}/${VENV_BIN}/python ${PYPATH}/download_samples.py --url ${URL} --langs "${LANGS[@]}"
}

openPage() {
  if [ $# != 1 ]; then
    echo "Usage: $0 <PROB_NUMBER>" 1>&2
    exit 1
  fi

  declare -r PROB_NUMBER=$1

  open "https://atcoder.jp/contests/${PROB_NUMBER%_*}/tasks/${PROB_NUMBER}"
}

lint() {
  if [ $# != 2 ]; then
    echo "Usage: $0 <LANG> <problem_number>" 1>&2
    exit 1
  fi

  declare -r L=$1
  declare -r PROBLEM=$2

  declare -r SOURCE=$(sourcePath ${L} ${PROBLEM})

  isExist ${SOURCE}
  if [ ${L} == ${LANGS[0]} ]; then
    # FIXME(fhiyo): Use lint tool (sblint not running in my environment)
    # sblint: https://github.com/fukamachi/sblint
    # sblint ${SOURCE}
    echo "Under construction..."
    :
  elif [ ${L} == ${LANGS[1]} ]; then
    hlint ${SOURCE}
  elif [ ${L} == ${LANGS[2]} ]; then
    ${PYPATH}/${VENV_BIN}/flake8 ${SOURCE}
  elif [ ${L} == ${LANGS[3]} ]; then
    cpplint ${SOURCE}
  else
    echo "LANG must be one of the following: ${LANGS}" 1>&2
    exit 1
  fi
}

clean() {
  source_dirs=$(find ./src/{clisp,haskell,py3,cpp17,java} -mindepth 1 -maxdepth 1 -type d)
  for program_dir in ${source_dirs}; do
    program=$(basename ${program_dir})
    pushd ${program_dir} >/dev/null
    \rm ${program} ${program}.hi ${program}.o 2>/dev/null
    \rm -rf __pycache__ ${program}.pyc 2>/dev/null
    \rm ${program}.lib ${program}.fas 2>/dev/null
    \rm a.out 2>/dev/null
    popd > /dev/null
  done
}

### Main

SOURCE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ $# == 0 ]; then
  usage
  exit 1
fi

# Move directory to this script file exists
# WARN: Work only not exists symbolic link!
cd ${SOURCE_DIR}

containsElement "$1" "${LANGS[@]}"
if [[ $? -eq 0 ]]; then
  readonly lang="$1"
  shift
fi

# analyse optional arguments
for opt in "$@"; do
  case "${opt}" in
    '-c' | '--clean' )
      clean
      ;;

    '-e' | '--edit' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      edit ${lang} ${prob_number}
      ;;

    '-g' | '--git-add' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      addToGit ${lang} ${prob_number}
      ;;

    '-h' | '--help' )
      usage
      exit 0
      ;;

    '-l' | '--lint' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      lint ${lang} ${prob_number}
      ;;

    '-m' | '--make-env' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires contest name as argument -- $1" 1>&2
        exit 1
      fi
      url="$2"
      shift 2
      makeEnv ${url}
      ;;

    '--open' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      openPage ${prob_number}
      ;;

    '-r' | '--run' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      run ${lang} ${prob_number}
      ;;

    '-a' | '--all-test' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      allTest ${lang} ${prob_number}
      ;;

    '-t' | '--test' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      if [[ -z "${3:-}" ]] || [[ "${3:-}" =~ ^-+ ]]; then
        echo "$0: option requires test number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      test_number="$3"
      shift 3
      test_ ${lang} ${prob_number} ${test_number}
      ;;

     '--copy' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      copyCode ${lang} ${prob_number}
      ;;

     '--cc' | '--copy_code_and_open_page' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      copyCodeAndOpenPage ${lang} ${prob_number}
      ;;

    '-i' | '--add-input' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      addInput ${prob_number}
      ;;

     '-o' | '--add-output' )
      if [[ -z "${2:-}" ]] || [[ "${2:-}" =~ ^-+ ]]; then
        echo "$0: option requires problem number as argument -- $1" 1>&2
        exit 1
      fi
      prob_number="$2"
      shift 2
      addOutput ${prob_number}
      ;;

  esac
done
