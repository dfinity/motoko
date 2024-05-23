#!/usr/bin/env bash

#
# This script wraps PocketIC to
#
# * extract the methods calls from comments in the second argument
#   (typically the test source files)
# * adds the right canister ids as the destination to these calls
# * writes canister cycles balance to file descriptor 222
#   (for run.sh -p; post-processing happening in run.sh)
#

if [ -z "$1" ]
then
  echo "Usage: $0 <name>.wasm [call-script]"
  exit 1
fi

export LANG=C.UTF-8

# on darwin, I have seen
#   thread 'MR Batch Processor' has overflowed its stack
# and this helps (default is 2MB)
export RUST_MIN_STACK=$((10*1024*1024))

# PocketIC creates canisters with this ID:
WASM="${1}"
ID="tqzl2-p7777-77776-aaaaa-cai"
BASE64ID="///////AAAABAQ=="

INSTANCE_ID=$(curl -X POST -H 'Content-Type: application/json' http://localhost:8000/instances -d '{"nns": {"state_config": "New", "instruction_config": "Production", "dts_flag": "Enabled"}, "sns": {"state_config": "New", "instruction_config": "Production", "dts_flag": "Enabled"}, "ii": null, "fiduciary": null, "bitcoin": null, "system": [], "application": [{"state_config": "New", "instruction_config": "Production", "dts_flag": "Enabled"}]}' | jq -r ".Created.instance_id")
PIC_URL="http://localhost:8000/instances/${INSTANCE_ID}"
PIC_INGRESS="${PIC_URL}/update/execute_ingress_message"
PIC_QUERY="${PIC_URL}/read/query"

CREATE_PAYLOAD=$(echo "(record {})" | didc encode | xxd -r -p | base64 | tr -d '\n')

curl -X POST -H "Content-Type: application/json" "${PIC_INGRESS}" -d "{\"sender\": \"BA==\", \"canister_id\": \"\", \"effective_principal\": {\"CanisterId\": \"${BASE64ID}\"}, \"method\": \"provisional_create_canister_with_cycles\", \"payload\": \"${CREATE_PAYLOAD}\"}"

INSTALL_PAYLOAD=$(echo "(record {
  mode=variant { install };
  canister_id=principal\"${ID}\";
  wasm_module=blob\"$(hexdump -ve '1/1 "%.2x"' "${WASM}" | sed 's/../\\&/g')\";
  arg=blob\"DIDL\00\00\";
})" | didc encode | xxd -r -p | base64 | tr -d '\n')

curl -X POST -H "Content-Type: application/json" "${PIC_INGRESS}" -d "{\"sender\": \"BA==\", \"canister_id\": \"\", \"effective_principal\": {\"CanisterId\": \"${BASE64ID}\"}, \"method\": \"install_code\", \"payload\": \"${INSTALL_PAYLOAD}\"}"

UPGRADE_PAYLOAD=$(echo "(record {
  mode=variant { upgrade };
  canister_id=principal\"${ID}\";
  wasm_module=blob\"$(hexdump -ve '1/1 "%.2x"' "${WASM}" | sed 's/../\\&/g')\";
  arg=blob\"DIDL\00\00\";
})" | didc encode | xxd -r -p | base64 | tr -d '\n')

INGRESS_CMD='q#curl -X POST -H "Content-Type: application/json" "'"${PIC_INGRESS}"'" -d "{\"sender\": \"BA==\", \"canister_id\": \"'"${BASE64ID}"'\", \"effective_principal\": \"None\", \"method\": \"# . "$1" . q#\", \"payload\": \"# . "$2" . q#\"}"#'
QUERY_CMD='q#curl -X POST -H "Content-Type: application/json" "'"${PIC_QUERY}"'" -d "{\"sender\": \"BA==\", \"canister_id\": \"'"${BASE64ID}"'\", \"effective_principal\": \"None\", \"method\": \"# . "$1" . q#\", \"payload\": \"# . "$2" . q#\"}"#'
UPGRADE_CMD='q#curl -X POST -H "Content-Type: application/json" "'"${PIC_INGRESS}"'" -d "{\"sender\": \"BA==\", \"canister_id\": \"\", \"effective_principal\": {\"CanisterId\": \"'"${BASE64ID}"'\"}, \"method\": \"# . "install_code" . q#\", \"payload\": \"# . "'"${UPGRADE_PAYLOAD}"'" . q#\"}"#'

if [ -n "$2" ]
then
  eval $(LANG=C perl -ne 'print '"${INGRESS_CMD}"' . " && " if m,^//CALL ingress ([^ ]*) (.*),;print '"${QUERY_CMD}"' . " && " if m,^//CALL query ([^ ]*) (.*),;print '"${UPGRADE_CMD}"' . " && " if m,^//CALL upgrade,;' $2) true
fi

PIC_CYCLES="${PIC_URL}/read/get_cycles"

REMAINING_CYCLES=$(curl -X POST -H "Content-Type: application/json" "${PIC_CYCLES}" -d "{\"canister_id\": \"${BASE64ID}\"}" | jq -r ".cycles")
USED_CYCLES="$((100000000000000-${REMAINING_CYCLES}))"
echo ${USED_CYCLES} > /dev/fd/222
