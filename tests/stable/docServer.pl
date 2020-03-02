
/* debug.pl Start the Prolog Documentation generator on this project

Starts the documentation server on clio source code.
Documentation on local code can be accessed at localhost:4040
For more documentation on Prolog Source Code Documentation see:
http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/pldoc.html%27)

Open browser with `doc_browser.`
@license MIT

*/

:- [clioStart].              % load your program

:- doc_server(4040,[]). % Start PlDoc at port 4040
:- portray_text(true).  % Enable portray of strings
:- doc_browser.

