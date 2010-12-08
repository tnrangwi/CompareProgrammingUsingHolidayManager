#!/usr/bin/python
#FIXME: -Error handling
#       -Bind to request from all addresses
"""
This is the holiday server. It provides a generic interface to a holiday calender for
multiple users. Communication is done via an abstract interface, currently using NC::SocketServer.
See the man page holiday for further details. Software is distributed under the MIT license.


Copyright (c) 2006-2010: Thorsten Rangwich

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
"""
import sys, os, logging, ConfigParser, logging, SocketServer, socket
from glob import glob
from os import F_OK, R_OK, W_OK


#########################################################
# Helper classes ########################################
#########################################################

class hContainer:
  "Container class for global context"

  htable = {};
  port = 2001
  privileged = None
  logfile = 'logfile.log'
  logger = None

  __init_done__ = False

  def __init__(self):
#FIXME: Log file name is fix, logging is done to root logger
#       If this shall be given in cfg file name, we must
#       remove load_usr here and call it externally.
    logger = logging.getLogger('')
    self.load_cfg()
    self.load_usr()

  def load_cfg(self):
    "load global configuration file"
    if self.__init_done__: return None
    else: self.__init_done__ = True

    if os.access('holiday.conf', F_OK): #no -r function using euid as in perl???
      try:
	fd = open('holiday.conf')
	cfg = ConfigParser.ConfigParser()
	cfg.readfp(fd)
	if cfg.has_section('global'):
	  if cfg.has_option('global', 'port'):
	    self.port = cfg.getint('global', 'port')
	  if cfg.has_option('global', 'privileged'):
	    self.privileged = cfg.get('global', 'privileged').split()
	fd.close()
	fd = None
	
      except IOError:
	pass #defaults set above

  def load_usr(self):
    "Loads currently saved holiday calenders for all users."
    for uf in glob('*.usr'):
#try:
      fd = file(uf, 'r')
#except IOError:
      u = uf[0 : -4]
      line = fd.readline()
      self.htable[u] = {}
      self.htable[u]['group'] = line[0:-1] #chomp
      self.htable[u]['times'] = {}
      while True:
	line = fd.readline()
#EOF check?
	if len(line) == 0:
	  break
	line = line[0:-1] #chomp
	d = line.split('|')
	self.htable[u]['times'][int(d[0])] = int(d[1])
      fd.close()
    if not self.htable.has_key('gast'): #default entry
      self.htable['gast'] = { 'group' : 'N', 'times' : { 12005 : 6, 3202005 : 2 } };

  def save_usr(self):
    "Save user timetables to config files."
#error check? try / catch?
#was kommt da bei integer raus?
    for k, v in self.htable.iteritems():
      fd = open(k + '.usr', 'w')
      fd.write(v['group'] + '\n')
      for d, l in v['times'].iteritems():
	fd.write(str(d) + '|' + str(l) + '\n')
      fd.close()
      fd = None


#########################################################

class hSocketServer(SocketServer.TCPServer):
  "Minimal extension of TCPServer to provide context for application"

  shutdown = False
  ctx = None
  empty_char = "\0"

  def __init__(self, rqh_class, ct):
#    SocketServer.TCPServer.__init__(self, (socket.INADDR_ANY, ct.port), rqh_class)
    SocketServer.TCPServer.__init__(self, ('0.0.0.0', ct.port), rqh_class)
    self.ctx = ct

  def format_response(self, s):
    ret = None
    if s == None or len(s) == 0: ret = self.empty_char
    else: ret = s
    return ret + "\r\n"

#########################################################

class hRequestHandler(SocketServer.StreamRequestHandler):
  """
     (Necessary) custom request handler class for handling
     server requests. We do not separate communication
     from contents because communication is nearly nothing
     with Python's StreamRequestHandler
  """

  def handle(self):
    """
     Unfortunately everything needs to be done here. Get
     request from client (one line), handle request and
     send response.
    """
    htable = self.server.ctx.htable
    logger = logging.getLogger('')
    req = self.rfile.readline()
    req = req[0:-2] #truncate \r\n
    data = req.split('|')
    try:
      if data[0] == 'test':
	 self.wfile.write('response\n')
	 self.wfile.write(req)
      elif data[0] == 'shutdown':
	self.server.shutdown = True
      elif data[0] == 'addh':
	if htable.has_key(data[1]):
	  htable[data[1]]['times'][data[2]] = data[3]
	  logger.log("Created/changed holiday: U:" + data[1] + ",S:" + data[2] + ",C:" + data[3] + ".")
	  self.wfile.write(self.server.format_response(""))
	else:
	  self.wfile.write(self.server.format_response('User ' + data[1] + ' does not exist, you are not allowed to do this'))
      elif data[0] == 'delh':
	logger.log('Deleted holiday: U:' + data[1] + ',S:' + data[2])
	del htable[data[1]]['times'][data[2]]
	self.wfile.write(self.server.format_response(""))
      elif data[0] == 'getu':
	if htable.has_key(data[1]):
	  for k, v in htable[data[1]]['times'].iteritems():
	    self.wfile.write(self.server.format_response(str(k) + '|' + str(v)))
      elif data[0] == 'geta':
	for u, d in htable.iteritems():
	  for k, v in d['times'].iteritems():
	    self.wfile.write(self.server.format_response(u + '|' + str(k) + '|' + str(v)))
      elif data[0] == 'addu':
	if self.server.ctx.privlist is None or self.client_address[0] in self.server.ctx.privlist:
	  htable[data[1]] = {}
	  htable[data[1]]['group'] = data[2]
	  htable[data[1]]['times'] = {}
	  self.server.format_response("")
	else:
	  logger.warn("getu not allowed from non privileged connection...")
	  self.server.format_response("Non privileged connection, you are not allowed to do this")
      else:
	self.server.format_response("Unknown command:" + data[0] + ".")
    except IndexError:
      self.wfile.write(self.server.format_response('Incomplete command:' + req))
    self.wfile.write("\r\n") #terminate output

###################################
# MAIN STARTS
###################################
  
#Create root logger
logger = logging.getLogger('') #get root logger
logger.setLevel(logging.INFO) #override default 'WARNING'
t = logging.FileHandler('logfile.log')
t.setFormatter(logging.Formatter('%(name)s %(levelname)s %(module)s %(asctime)s %(message)s'))
logger.addHandler(t) #add one handler, otherwise it would be created implicitly in 1st msg
t = None
del t
logger.info('Startup')

server_ctx = hContainer()

sock = hSocketServer(hRequestHandler, server_ctx)
while not sock.shutdown:
  sock.handle_request()
sock.server_close()

server_ctx.save_usr() #implies we wish to save after every quit of main loop
