"Java/Maven Configuration
set makeprg=mvn
set errorformat=[ERROR]\ %f:[%l\\,%v]\ %m
function ClassTest()
  call MavenClassTest()
endfunction

"Project Indentation Settings
set softtabstop=0       " Controls mix of tabs and spaces, 0 disables it
set expandtab         " Controls whether to expand tab to the corresponding # of chars
set shiftwidth=4        " Number of spaces to use for each step of (auto)indent
set tabstop=4           " Indentation size

