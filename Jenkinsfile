pipeline {
    agent any
    stages {
        stage('Build and test') {
            steps {
                sh 'nix build'
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
