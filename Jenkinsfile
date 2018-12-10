pipeline {
    agent any
    stages {
        stage ('git submodule') {
            steps {
                sh 'git submodule update --init --recursive'
                sh 'git clone --recursive git@github.com:dfinity-lab/dev nix/dev'
                sh 'git -C nix/dev checkout 3fd77aeeae1cb35e2bd86d9a49996cd1aedf57c5'
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
