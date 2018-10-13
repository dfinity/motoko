pipeline {
    agent any
    stages {
        stage ('git submodule') {
            steps {
                sh 'git submodule update --init --recursive'
                sh 'git clone --recursive git@github.com:dfinity-lab/dev nix/dev'
                sh 'git -C nix/dev checkout 3582a30d2687b94f0028396ecb33166a9815117b'
                sh 'git -C nix/dev submodule update --init --recursive'
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
