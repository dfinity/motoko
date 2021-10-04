import ssl
import subprocess
import sys
import urllib.request

package_name = sys.argv[1]
package_version = sys.argv[2]

package_url = f'https://crates.io/api/v1/crates/{package_name}/{package_version}/download'
package_file_name = f'{package_name}-{package_version}.gz'

print(f'Vendoring package "{package_name}" version = "{package_version}"')
print(f'Package URL: {package_url}')

urllib.request.urlretrieve(package_url, package_file_name)

print(f'Done downloading package: {package_file_name}')

subprocess.run(['tar', '-xvf', package_file_name])

checksum_json_file_path = f'{package_name}-{package_version}/.cargo-checksum.json'

print(f'Creating file "{checksum_json_file_path}"')

with open(checksum_json_file_path, 'w') as cargo_checksum:
    cargo_checksum.write('{"files":{}}')
