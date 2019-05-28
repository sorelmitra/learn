from pywinauto import application;

app = application.Application();
app.start_('notepad.exe');
app.Notepad.MenuSelect('help->aboutnotepad');
print "hi there";
