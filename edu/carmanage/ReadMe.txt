CarManage

Design Patterns folosite per cerințe

1. 

Chain of Responsibility

Fiecare specialist este un responsabil din lanț (InspectorMasina). Mașina este trecută pe la fiecare specialist din lanț, care își face verificările și completează în lista de reparații.

În plus:

Pentru a simula și controla din program operațiile de inspecție se folosește o listă de PROBLEME (enum) care este folosită pentru a indica ce probleme are mașina. Lista de probleme se completează la începutul programului. (În viața reală, problemele ar fi fost completate de fiecare specialist care inspectează mașina.)

În program, fiecare specialist/inspector cercetează lista de probleme din Mașină și le recunoaște pe cele care aparțin specialității lui.

Command

Fiecare Reparație din listă este o comandă. Fiecare comandă / Reparație concretă ține un Executant, căruia îi delegă execuția Reparației.


2.

Strategy

Fiecare Executant al unui tip de reparație este implementat ca un Concrete Strategy.


3.

State

În funcție de starea mașinii (în reparații sau gata), un Concrete State este folosit pentru a returna informația corespunzătoare (reparații rămase și durata lor, respectiv fișa de reparații și data preluării).

Composite

Fiecare Concrete State returnează o Informație, care este o interfață.
Fiecare Informație concretă poate fi simplă (ex. Informație Durată) sau compusă (ex. Informație Gata, Informație În Lucru).
O Informație simplă se afișează direct.
O Informație compusă delegă afișarea la sub-informațiile conținute.


4.

Decorator

Mașina este reprezentată ca o clasă cu o colecție de elemente.
Fiecare element poate fi simplu sau "tunabil".

Fiecare element "tunabil" (ex. Volan) implementează interfața "Tunable".
Fiecare element "tunat" concret (ex. Volan de Lemn) implementează interfața "Tuned" (Decorator), care extinde Tunable. Elementul tunat concret adaugă starea și operațiile corespunzătoare.

Mașina este apoi reconstruită folosind elementele tunate. O clasă "TuningSystem" ține evidența acestor tunări și construiește/modifică (simulează) mașina cu elementele tunate.

Memento

La simulare, atunci când se prezintă mașina cu decoratorii aplicați, înainte de aplicarea decoratorilor, TuningSystem cere starea Mașinii sub forma unei Memento. Apoi Mașina este modificată în simulare.

Dacă clientul cere să revină la starea originală, TuningSystem dă înapoi Mașinii obiectul Memento primit inițial. Mașina se restaurează singură aplicând setările din Memento.

