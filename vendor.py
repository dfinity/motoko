import os
import ssl
import subprocess
import sys
import urllib.request

def sha256sum(path):
    output = subprocess.run(['sha256sum', path], capture_output=True).stdout
    print(path + " output: " + str(output))
    return output.split()[0]

package_name = sys.argv[1]
package_version = sys.argv[2]

package_url = f'https://crates.io/api/v1/crates/{package_name}/{package_version}/download'
package_file_name = f'{package_name}-{package_version}.gz'

print(f'Vendoring package "{package_name}" version = "{package_version}"')
print(f'Package URL: {package_url}')

urllib.request.urlretrieve(package_url, package_file_name)

print(f'Done downloading package: {package_file_name}')

package_checksum = sha256sum(package_file_name)

print(f'Package checksum: {package_checksum}')

subprocess.run(['tar', '-xvf', package_file_name])

package_dir_path = f'{package_name}-{package_version}'

checksum_json_file_path = f'{package_dir_path}/.cargo-checksum.json'

print(f'Creating file "{checksum_json_file_path}"')

with open(checksum_json_file_path, 'w') as cargo_checksum:
    cargo_checksum.write('{"package":"{%s}","files":{}}' % package_checksum)
