#!/usr/bin/env bash

sed "s/^....-..-.. ..:..:............ UTC: \[Canister tqzl2-p7777-77776-aaaaa-cai\]/debug.print:/" | \
sed "s/^{\"Ok\":{\"Reply\":\"\(.*\)\"}} ingress$/ingress Completed: Reply: \1/" | \
sed "s/^{\"Ok\":{\"Reject\":\"\(.*\)\"}} ingress$/ingress Completed: Reject: \1/" | \
sed "s/^{\"Err\":\(.*\)} ingress$/ingress Err: \1/" | \
sed "s/^{\"Ok\":{\"Reply\":\"\(.*\)\"}} query$/Ok: Reply: \1/" | \
sed "s/^{\"Ok\":{\"Reject\":\"\(.*\)\"}} query$/Ok: Reject: \1/" | \
sed "s/^{\"Err\":\(.*\)} query$/Err: \1/"
