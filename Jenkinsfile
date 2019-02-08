pipeline {
    agent any
    stages {
        stage ('git submodule') {
            steps {
                sh 'git submodule update --init --recursive'
                sh 'git clone --recursive git@github.com:dfinity-lab/dev nix/dev'
                sh 'git -C nix/dev checkout e588f0efa687667076dfab52fddde6ff29e0d82c'
                sh 'git -C nix/dev submodule update --init --recursive'
            }
        }

        stage('Build (native)') {
            steps {
                sh 'nix-build -A native --arg test-dvm true'
            }
        }
        stage('Test (native)') {
            steps {
                sh 'nix-build -A native_test --arg test-dvm true'
            }
        }
        stage('Build and test (js)') {
            steps {
                sh 'nix-build -A js'
            }
        }
    }
    // Workspace Cleanup plugin
    post {
        always {
            cleanWs()
        }
    }
}
