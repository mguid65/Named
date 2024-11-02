import subprocess


def check_compiler_exists(compiler):
    try:
        subprocess.check_call([compiler, "--version"], stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        return True
    except subprocess.CalledProcessError:
        pass
    return False
