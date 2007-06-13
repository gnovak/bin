import unittest;
from mail import *

# Test message size function
def some(args): return reduce(lambda x,y: x or y, args, False)

class ImapTest(unittest.TestCase):
    def testFolderName(self):
        self.assertEqual(imapFolderName("INBOX"), "IN")
        self.assertEqual(imapFolderName("INBOX.foo-bar.baz"), "foo-bar.baz")

class ImapServerTest(unittest.TestCase):
    def setUp(self):
        server, user, password = imapServers[0]
        self.imap = imaplib.IMAP4(server)
        self.imap.login(user, password)
        self.imap.select()
        
    def tearDown(self):
        # I would have thought that the close command should be given,
        # but returns an error.
        # self.imap.close()
        self.imap.logout()

    def testMids(self):
        response, data = self.imap.search(None, 'ALL')
        ids = data[0].split(' ')
        # these should all be numbers
        for id in ids:
            int(id)

        response, data = self.imap.list()
        for datum in data:
            self.assertEqual(len(datum.split('"')), 5)

        # The next to last entry of each split should look like a
        # mailbox.  For now this means that it starts with "INBOX"
        for datum in data:
            self.assertTrue(datum.split('"')[3].startswith("INBOX"))

    def testFolders(self):
        # I rely on the format of the response to imap.list() being:
        # '(Flags) "." "name"'
        # Eg:
        # '(\\Noselect \\HasChildren) "." "INBOX.spam"'
        # There should be five entries in the list that results from splitting on "
        response, data = self.imap.list()
        for datum in data:
            self.assertEqual(len(datum.split('"')), 5)

        # The first entry should be mailbox flags.  Make sure that at
        # least one folder has each of the given flags, so I know if
        # their format has changed        

        flagsList = [datum.split('"')[0] for datum in data]
        for flagName in ('Marked', 'HasNoChildren', 'Noselect', 'HasChildren'):
            self.assertTrue( some( [flags.find(flagName) != -1
                                    for flags in flagsList]))
        
        for datum in data:
            self.assertTrue(datum.split('"')[3].startswith("INBOX"))

        # The next to last entry of each split should look like a
        # mailbox.  For now this means that it starts with "INBOX"
        for datum in data:
            self.assertTrue(datum.split('"')[3].startswith("INBOX"))        
        
class MailTest(unittest.TestCase):
    
    def oneOnly(self, args):
        return map(bool, args).count(True) == 1
    
    def testOneOnly(self):
        self.assertFalse(self.oneOnly([False, False, False]))
        self.assertTrue (self.oneOnly([False, False,  True]))
        self.assertTrue (self.oneOnly([False,  True, False]))
        self.assertFalse(self.oneOnly([False,  True,  True]))
        self.assertTrue (self.oneOnly([ True, False, False]))
        self.assertFalse(self.oneOnly([ True, False,  True]))
        self.assertFalse(self.oneOnly([ True,  True, False]))
        self.assertFalse(self.oneOnly([ True,  True,  True]))
        
    def oneMatchOnly(self, name):
        self.oneOnly([inboxp(name), subInboxp(name),
                      folderp(name), generalp(name),
                      listp(name)])

    def testNames(self):
        for name in ("IN", "INB", "INBOX",
                     "IN-asdf",
                     "asdf", "spam-discussion",
                     "recd", "sent", "recd-asdf", "sent-asdf",
                     "spam", "spam.200asdf", "trash",
                     "LISTasdf"):
            self.oneMatchOnly(name)

        self.assertTrue(inboxp("IN"))
        self.assertFalse(inboxp("INB"))
        self.assertFalse(inboxp("INBOX"))

        self.assertFalse(subInboxp("IN"))
        self.assertTrue(subInboxp("INBOX"))
        self.assertTrue(subInboxp("INB"))
        self.assertTrue(subInboxp("IN-asdf"))

        self.assertTrue(folderp("asdf"))
        self.assertTrue(folderp("spam-discussion"))

        self.assertTrue(generalp("recd"))
        self.assertTrue(generalp("sent"))
        self.assertTrue(generalp("recd-asdf"))
        self.assertTrue(generalp("recd-asdf"))

        self.assertTrue(spamp("spam"))
        self.assertTrue(spamp("spam.200asdf"))
        self.assertTrue(spamp("trash"))
        self.assertFalse(spamp("spam-discussion"))

        self.assertTrue(listp("LISTasdf"))


def suite():
    suites = [unittest.TestLoader().loadTestsFromTestCase(test)
              for test in (MailTest,ImapTest,
                           ImapServerTest,
                           )]
    return unittest.TestSuite(suites)

def test():
    unittest.TextTestRunner().run(suite())

def itest():
    suite().debug()
