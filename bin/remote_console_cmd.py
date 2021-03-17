import os
import sys

workdir = sys.argv[1]
project_dir = os.path.dirname(workdir)


def get_dgiot_config(key):
    with open(os.path.join(project_dir, 'etc', 'emqx.conf'), 'r') as f:
        for line in f.readlines():
            if line.startswith(key):
                return line.strip('\n').split('=')[1].strip()


def remote_node(node):
    name, ip = node.split('@')
    return name + '2' + '@' + ip


def ekka():
    for plugin in os.listdir(os.path.join(project_dir, 'lib')):
        if plugin.startswith('ekka'):
            return os.path.join(project_dir, 'lib', plugin, 'ebin')


cmd = "erl" \
      " -name {remote_node} " \
      " -setcookie {cookie}" \
      " -start_epmd false" \
      " -epmd_module ekka_epmd" \
      " -proto_dist ekka" \
      " -pa {ekka}" \
      " -remsh {node}".format(
    remote_node=remote_node(get_dgiot_config('node.name')),
    node=get_dgiot_config('node.name'),
    ekka=ekka(),
    cookie=get_dgiot_config('node.cookie')
)

print(cmd)
