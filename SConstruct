INSTALL_DIR = 'bin'

env = Environment()
Export('env')

bits = SConscript(['server/SConscript', 'client/SConscript'])

inst = env.Install(INSTALL_DIR, bits)
