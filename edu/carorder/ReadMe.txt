CarOrder

Design Patterns folosite per cerințe

1.

Prototype

Fiecare model de bază de mașină are propriul ei prototip:

                                CarPrototype
                                ------------
                                + clone()
                                      |
   ------------------------------------------------------------------
   |                       |                    |                   |
SedanCarPrototype   CoupeCarPrototype   WagonCarPrototype   VanCarPrototype
-----------------   -----------------   -----------------   ---------------
+ clone()           + clone()           + clone()           + clone()
- baseItems         - baseItems         - baseItems         - baseItems

     CarPrototypeCache
---------------------------
+ CarPrototypeCache()
+ getPrototypeInstance(String id)


2.

Singleton pentru generarea de numere de șasiu.

CarChassisNumberGenerator
-------------------------
- CarChassisNumberGenerator()
+ getInstance(): CarChassisNumberGenerator
+ createChassisNumber(): String


3.

Builder și Factory Method pentru adăugarea de opțiuni la mașină.

            Car
---------------------------
- items: ArrayList<CarItem>
+ addOption(CarItem item)


                       CarItem <<abstract>>
                       --------------------
                       + getPrice()
                       + getName()
                       + show(Writer w)
                              |
    -----------------------------------------------------
    |                         |                         |
SportSeatsCarItem    RainSensorCarItem       ChasisCarItem <<abstract>>
-----------------    ----------------        --------------------------
+ getPrice()         + getPrice()                        |
+ getName()          + getName()                         |
                                             -----------------------------
                                             |                           |
                                 SedanChasisCarItem               WagonChasisCarItem
                                 ------------------               ------------------
                                 + getPrice()                     + getPrice()
                                 + getName()                      + getName() 

       CarBuilder
--------------------------
+ createCar(prototype, String[] options) --->
                                              item = carItemFactory.createCarItem(string)
                                              car.addItem(item)

