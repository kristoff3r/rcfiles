import xchat
import re

__module_name__ = "autojoin"
__module_version__ = "1.0"
__module_description__ = "Autojoining"


chanlist = ["#zomg_pwnies_trusted"]

def join_channel(c):
    xchat.command("join " + c)

def invite_hook(word, word_eol, userdata):
    join_channel(word[0])

def notice_hook(word, word_eol, userdata):
    if word[0] == "NickServ" and word[1].startswith("You are now identified"):
        for c in chanlist:
            xchat.command("chanserv invite " + c)
    elif word[0] == "ChanServ" and word[1].endswith(" is currently empty."):
        for c in re.findall(r"#[a-zA-Z_]+", word[1]):
            join_channel(c)


xchat.hook_print("Invited", invite_hook)
xchat.hook_print("Notice", notice_hook)
