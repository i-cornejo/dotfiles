from subprocess import run, PIPE

def get_password(acc):
    command = ['emacsclient', '--eval',
               f'(my/lookup-password :user "{acc}")']
    return run(command, capture_output=True).stdout.strip(b'"\n')

