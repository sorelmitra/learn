/******/ (() => { // webpackBootstrap
/******/ 	"use strict";
/******/ 	var __webpack_modules__ = ({

/***/ "./src/index.ts":
/*!**********************!*\
  !*** ./src/index.ts ***!
  \**********************/
/***/ (function() {


var __decorate = (this && this.__decorate) || function (decorators, target, key, desc) {
    var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
    if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
    else for (var i = decorators.length - 1; i >= 0; i--) if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
};
var __metadata = (this && this.__metadata) || function (k, v) {
    if (typeof Reflect === "object" && typeof Reflect.metadata === "function") return Reflect.metadata(k, v);
};
const first = () => {
    console.log("first(): factory evaluated");
    return function (target, propertyKey, descriptor) {
        console.log("first(): called", target, propertyKey, descriptor);
    };
};
function second() {
    console.log("second(): factory evaluated");
    return function (target, propertyKey, descriptor) {
        console.log("second(): called");
    };
}
class ExampleClass {
    greet() {
        console.log("Hello, there");
    }
}
__decorate([
    first(),
    second(),
    __metadata("design:type", Function),
    __metadata("design:paramtypes", []),
    __metadata("design:returntype", void 0)
], ExampleClass.prototype, "greet", null);
new ExampleClass().greet();


/***/ })

/******/ 	});
/************************************************************************/
/******/ 	
/******/ 	// startup
/******/ 	// Load entry module and return exports
/******/ 	// This entry module is referenced by other modules so it can't be inlined
/******/ 	var __webpack_exports__ = {};
/******/ 	__webpack_modules__["./src/index.ts"]();
/******/ 	
/******/ })()
;
//# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYnVuZGxlLmpzIiwibWFwcGluZ3MiOiI7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7O0FBQUEsTUFBTSxLQUFLLEdBQUcsR0FBRyxFQUFFO0lBQ2pCLE9BQU8sQ0FBQyxHQUFHLENBQUMsNEJBQTRCLENBQUMsQ0FBQztJQUMxQyxPQUFPLFVBQVUsTUFBVyxFQUFFLFdBQW1CLEVBQUUsVUFBOEI7UUFDL0UsT0FBTyxDQUFDLEdBQUcsQ0FBQyxpQkFBaUIsRUFBRSxNQUFNLEVBQUUsV0FBVyxFQUFFLFVBQVUsQ0FBQyxDQUFDO0lBQ2xFLENBQUMsQ0FBQztBQUNKLENBQUM7QUFFRCxTQUFTLE1BQU07SUFDYixPQUFPLENBQUMsR0FBRyxDQUFDLDZCQUE2QixDQUFDLENBQUM7SUFDM0MsT0FBTyxVQUFVLE1BQVcsRUFBRSxXQUFtQixFQUFFLFVBQThCO1FBQy9FLE9BQU8sQ0FBQyxHQUFHLENBQUMsa0JBQWtCLENBQUMsQ0FBQztJQUNsQyxDQUFDLENBQUM7QUFDSixDQUFDO0FBRUQsTUFBTSxZQUFZO0lBR2hCLEtBQUs7UUFDTixPQUFPLENBQUMsR0FBRyxDQUFDLGNBQWUsQ0FBQyxDQUFDO0lBQzVCLENBQUM7Q0FDRjtBQUhDO0lBRkMsS0FBSyxFQUFFO0lBQ1AsTUFBTSxFQUFFOzs7O3lDQUdSO0FBR0gsSUFBSSxZQUFZLEVBQUUsQ0FBQyxLQUFLLEVBQUUsQ0FBQzs7Ozs7Ozs7VUV0QjNCO1VBQ0E7VUFDQTtVQUNBO1VBQ0EiLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly9kZXAtaW5qZWN0aW9uLy4vc3JjL2luZGV4LnRzIiwid2VicGFjazovL2RlcC1pbmplY3Rpb24vd2VicGFjay9iZWZvcmUtc3RhcnR1cCIsIndlYnBhY2s6Ly9kZXAtaW5qZWN0aW9uL3dlYnBhY2svc3RhcnR1cCIsIndlYnBhY2s6Ly9kZXAtaW5qZWN0aW9uL3dlYnBhY2svYWZ0ZXItc3RhcnR1cCJdLCJzb3VyY2VzQ29udGVudCI6WyJjb25zdCBmaXJzdCA9ICgpID0+IHtcbiAgY29uc29sZS5sb2coXCJmaXJzdCgpOiBmYWN0b3J5IGV2YWx1YXRlZFwiKTtcbiAgcmV0dXJuIGZ1bmN0aW9uICh0YXJnZXQ6IGFueSwgcHJvcGVydHlLZXk6IHN0cmluZywgZGVzY3JpcHRvcjogUHJvcGVydHlEZXNjcmlwdG9yKSB7XG4gICAgY29uc29sZS5sb2coXCJmaXJzdCgpOiBjYWxsZWRcIiwgdGFyZ2V0LCBwcm9wZXJ0eUtleSwgZGVzY3JpcHRvcik7XG4gIH07XG59XG4gXG5mdW5jdGlvbiBzZWNvbmQoKSB7XG4gIGNvbnNvbGUubG9nKFwic2Vjb25kKCk6IGZhY3RvcnkgZXZhbHVhdGVkXCIpO1xuICByZXR1cm4gZnVuY3Rpb24gKHRhcmdldDogYW55LCBwcm9wZXJ0eUtleTogc3RyaW5nLCBkZXNjcmlwdG9yOiBQcm9wZXJ0eURlc2NyaXB0b3IpIHtcbiAgICBjb25zb2xlLmxvZyhcInNlY29uZCgpOiBjYWxsZWRcIik7XG4gIH07XG59XG4gXG5jbGFzcyBFeGFtcGxlQ2xhc3Mge1xuICBAZmlyc3QoKVxuICBAc2Vjb25kKClcbiAgZ3JlZXQoKSB7XG5cdGNvbnNvbGUubG9nKFwiSGVsbG8sIHRoZXJlXCIhKTtcbiAgfVxufVxuXG5uZXcgRXhhbXBsZUNsYXNzKCkuZ3JlZXQoKTtcbiIsIiIsIi8vIHN0YXJ0dXBcbi8vIExvYWQgZW50cnkgbW9kdWxlIGFuZCByZXR1cm4gZXhwb3J0c1xuLy8gVGhpcyBlbnRyeSBtb2R1bGUgaXMgcmVmZXJlbmNlZCBieSBvdGhlciBtb2R1bGVzIHNvIGl0IGNhbid0IGJlIGlubGluZWRcbnZhciBfX3dlYnBhY2tfZXhwb3J0c19fID0ge307XG5fX3dlYnBhY2tfbW9kdWxlc19fW1wiLi9zcmMvaW5kZXgudHNcIl0oKTtcbiIsIiJdLCJuYW1lcyI6W10sInNvdXJjZVJvb3QiOiIifQ==