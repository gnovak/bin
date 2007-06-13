### 
import os, types, time, re
import logging, cPickle
import mailbox, imaplib, smtplib, mailbox, email

# Astro-ph
# List-mail
# ham

DRY_RUN = False

ALL = 0; INBOX = 1; SUB_INBOX = 2; FOLDER = 3; GENERAL = 4; SPAM = 5; LIST = 6
MODE = INBOX

logFilename = '/Users/novak/mail.log'
successDictFilename = '/Users/novak/success-mail.dat'
# failureDictFilename = '/Users/novak/failure-mail.dat'

successDict = None
# failureDict = None

mboxDirs = ['/Users/novak/Mail', '/Users/novak/Mail-processed', '/Users/novak/Mail-backup',
            '/Users/novak/public-mail', '/Users/novak/dionysus-mail']

imapServers = [("imap.ucolick.org", "novak", "kupentorpt")]
IMAP_MAX_SIZE = 1000000

fromAddress = "novak@ucolick.org"
toAddress = "greg.novak@gmail.com"
smtpServer = "smtp.ucolick.org"
smtpPort = 587
smtpUser = "novak"
smtpPassword = "kupentorpt"

##################################################
## Utils
def every(args): return reduce(lambda x,y: x and y, args, True)

def can(obj, file, protocol=2):
    """More convenient interactive syntax for pickle"""
    if type(file) is types.StringType: f=open(file,'wb')
    else: f=file

    cPickle.dump(obj, f, protocol=protocol)

    if type(file) is types.StringType: f.close()

def uncan(file):
    """More convenient interactive syntax for pickle"""
    # If filename, should this read until all exhausted?
    if type(file) is type('string'): f=open(file, 'rb')
    else: f=file    

    obj = cPickle.load(f)

    if type(file) is types.StringType: f.close()

    return obj

simpleTypes = [types.BooleanType, types.ComplexType, types.FloatType,
               types.IntType, types.LongType, types.NoneType,
               types.StringType, types.StringTypes]

compositeTypes = [types.ListType, types.TupleType, types.DictType]

def rtype(obj, max=50):    
    def typesEqual(els):
        firstType = type(els[0])
        return every([ type(el) is firstType for el in els])
    def typesSimple(els):
        return every([ type(el) in simpleTypes for el in els])
    def name(obj):
        return type(obj).__name__
    def shape(obj):
        return str(len(obj))
    def contents(obj):
        if   type(obj) in [types.ListType, types.TupleType]: return obj
        elif type(obj) is types.DictType: return obj.values()
        return None

    if type(obj) in compositeTypes:
        if typesEqual(contents(obj)) and typesSimple(contents(obj)):
            return '%s of %s %s' % (name(obj), shape(obj), name(contents(obj)[0]))
        elif len(contents(obj)) > max:
            return ['%s of' % name(obj)] \
                   + [rtype(el) for el in contents(obj) [:max] ] \
                   + ['........']
        else: 
            return ['%s of' % name(obj)] \
                   + [rtype(el) for el in contents(obj)]         
    return name(obj)
    
##################################################
## Handle log of messages sent
def readLogDict(fn):
    try: return uncan(fn)
    except IOError:
        return {}
        
successDict = readLogDict(successDictFilename)
# failureDict = readLogDict(failureDictFilename)
        
##################################################
## Logging
# 
if DRY_RUN:    
    logging.basicConfig(level=logging.INFO,
                        format='%(created)s %(asctime)s %(levelname)s %(message)s')
else:
    logging.basicConfig(level=logging.INFO,
                        format='%(created)s %(asctime)s %(levelname)s %(message)s',
                        filename=logFilename)

##################################################
## Identify folders in various classes
def inboxp(name): return name == "IN"
def subInboxp(name): return name.startswith("IN") and not inboxp(name)
def folderp(name): return not (inboxp(name) or subInboxp(name)
                               or generalp(name) or spamp(name) or listp(name))
def generalp(name): return (name.startswith("sent") or 
                            name.startswith("recd"))
def spamp(name): return (name == "spam" or name == "trash" 
                         or name.startswith("spam.200"))
def listp(name): return name.startswith("LIST")

def processp(name):
    return (MODE == ALL or 
            MODE == INBOX and inboxp(name) or 
            MODE == SUB_INBOX and subInboxp(name) or 
            MODE == FOLDER and folderp(name) or 
            MODE == GENERAL and generalp(name) or 
            MODE == SPAM and spamp(name) or 
            MODE == LIST and listp(name))

##################################################
## 
theCount = [0]

def walkMboxDir(theDir):
    logging.info("****************************** mbox opening " + theDir)
    for root, dirs, files in os.walk(theDir):
        for file in files:
            if processp(file):
                handleMbox(os.path.join(root, file))
    logging.info("****************************** mbox closing " + theDir)

def handleMbox(theFile):
    logging.info("***** mbox opening " + theFile)

    for message in mailbox.mbox(theFile):
    # for message,junk in zip(mailbox.mbox(theFile), range(3)):
        # skip the internal messages in each mbox folder.. they
        # don't have message id's and generate spurious error messages
        if (message['Message-ID'] is None 
            and message['Subject'] 
            and message['Subject'].startswith("DON'T DELETE THIS MESSAGE -- "
                                              "FOLDER INTERNAL DATA")):
            continue
        
        if needsResent(message, theFile):
            sendMessage(message.as_string(), "mbox" + theFile,
                        message['Message-ID'])
            if not DRY_RUN:
                time.sleep(2)
            
    logging.info("***** mbox closing " + theFile)
    
##################################################
## IMAP
def imapFolderName(name):
    # Strip leading "INBOX." string, except for the INBOX folder
    # itself.  For that one, just strip the period.
    # This is solely for the purpose of dividing folders into classses as above
    if name == "INBOX":
        return "IN"
    return name.replace("INBOX.", "")    

def imapSelectableFolders(imap):
    response, data = imap.list()

    result = []
    for datum in data:
        flags, junk, junk, folder, junk = datum.split('"')
        if flags.find('Noselect') == -1:
            result.append(folder)
    return result
        
def imapMessages(imap):
    response, data = imap.search(None, 'ALL')
    return [int(s) for s in data[0].split(' ') if s != '']

def imapMessageSize(imap, mid):
    response, (data,) = imap.fetch(mid, '(RFC822.SIZE)')
    match = re.search(r'RFC822.SIZE ([0-9]+)', data)
    return int(match.groups()[0])

def imapServer(serverSpec):
    server, user, password = serverSpec
    logging.info("******************************Starting IMAP server %s" % server)

    try: 
        imap = imaplib.IMAP4(server)
        imap.login(user, password)
        imap.select()
        folders = imapSelectableFolders(imap)
    except Exception,e:
        logging.error("Failed to start IMAP session %s" % e)
        folders = []

    # for folder in folders:
    for folder in folders:
        if processp(imapFolderName(folder)):
            logging.info("***** Imap opening %s -- %s" % (server, folder))
            handleImapFolder(imap, folder)
            logging.info("***** Imap closing %s -- %s" % (server, folder))

    logging.info("******************************Imap finished with %s" % server)

def handleImapFolder(imap, folder):        
    try: 
        imap.select(folder)
        mids = imapMessages(imap)
    except Exception,e:
        logging.error("Couldn't open %s because %s" % (folder, e))
        mids = []

    # Get header, check message id
    for mid in mids:
        try:
            response = imap.fetch(mid, '(RFC822.HEADER)')
            code, ((echo, text), junk) = response
            message = email.message_from_string(text)

            if imapMessageSize(imap,mid) > IMAP_MAX_SIZE:
                logging.error("Message too big! (%s, %s, %s)" 
                              % (folder, mid, message['Message-ID']))
            elif needsResent(message, folder):
                if not DRY_RUN:
                    response = imap.fetch(mid, '(RFC822)')
                    # code, ((echo, text), junk) = response
                    text = response[1][0][1]
                    sendMessage(text, "imap" + "--" + folder,
                                message['Message-ID'])
                    time.sleep(2)
                else:
                    sendMessage("message", "imap" + "--" + folder,
                                message['Message-ID'])
        except Exception,e:
            logging.error("Problem with (%s, %s, %s) response %s because %s" 
                          % (folder, mid, message['Message-ID'],
                             rtype(response), e))
            print "ENTRY [0]", response[0]
            print "ENTRY [1][1]", response[1][1]
            print "ENTRY [1][2]", response[1][2]
            print "ENTRY [1][0][0]", response[1][0][0]
            print "ENTRY [1][0][1]", response[1][0][1]
            
            
        
def needsResent(message, locationSpec):
    if not 'Message-ID' in message:
        logging.error("Sending message in %s even though it has no message-id!" 
                         % locationSpec)
        return True
#         logging.error("Skipping message in %s because it has no message-id!" 
#                       % locationSpec)
#         return False

    mid = message['Message-ID']
    if mid in successDict:
        logging.info("Skipping %s in %s because it was already sent."
                     % (mid, locationSpec))
        return False

    return True

##################################################
## SMTP

incrementCount = [0]

def sendMessage(message, path, mid):
    # message should be a string
    try: 
        if not DRY_RUN:
            smtp = smtplib.SMTP(smtpServer, smtpPort)
            smtp.login(smtpUser, smtpPassword)
            # smtp.set_debuglevel(1)
            smtp.sendmail(fromAddress, toAddress, message)
            smtp.quit()

            successDict[mid] = (time.time(), path, mid)

            incrementCount[0] += 1
            if incrementCount[0] % 12 == 0:
                can(successDict, successDictFilename)
            
        logging.info("Sent " + path + str(mid))

    except Exception,e:
        # if not DRY_RUN:
            # failureDict[mid] = time.time()
            # can(failureDict, failureDictFile)
            
        logging.error("Failed to send " + path + str(mid) + "--" + str(e))

def main():
#     for mboxDir in mboxDirs:
#         walkMboxDir(mboxDir)

    for imapSpec in imapServers:
        imapServer(imapSpec)

    can(successDict, successDictFilename)
    return

##################################################
## Rickety code
## max size for dicts? ~ 30,000
## max pickled size?
## max string size?
def readlog():
    f = open('/Users/novak/mail.log')
    i=0
    result = {}
    for line in f:
        # if i==10: break
        i += 1
        if line.find('INFO Sent') != -1:
            match = re.search(r'(<.*>)', line)
            if match: 
                mid = match.groups()[0]
                result[mid] = 1 
        
    f.close()
    return result
