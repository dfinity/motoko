pipeline {
    agent any
    stages {
        stage('Build and test (native)') {
            steps {
                sh 'nix-build -A native'
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
