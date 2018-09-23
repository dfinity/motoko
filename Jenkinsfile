pipeline {
    agent any
    stages {
        stage ('git submodule') {
            steps {
                sh 'git submodule update --init --recursive'
                sh 'git clone --recursive git@github.com:dfinity-lab/dev-in-nix nix/dev-in-nix'
                sh 'git -C nix/dev-in-nix checkout 3bf560b328cf6308e02970055827c4f515058272'
                sh 'git -C nix/dev-in-nix submodule update --init --recursive'
            }
        }

        stage('Build and test (native)') {
            steps {
                sh 'nix-build -A native --arg test-dsh true'
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
