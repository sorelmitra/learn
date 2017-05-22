CarManage

Design Patterns folosite per cerințe

1. 

Chain of Responsibility

Fiecare specialist este un Responsabil din lanț. Mașina este trecută pe la fiecare specialist din lanț, care își face verificările și completează în lista de reparații.

Command

Fiecare reparație din listă este o comandă. Fiecare comandă concretă ține un Executant, căruia îi delegă execuția reparației.

2.

Strategy

Fiecare Executant al unui tip de reparație este implementat ca un Concrete Strategy.

3.

State

În funcție de starea mașinii (în reparații sau gata), un Concrete State este folosit pentru a returna informația corespunzătoare (reparații rămase și durata lor, respectiv fișa de reparații și data preluării).

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

