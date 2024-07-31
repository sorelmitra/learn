/******/ (() => { // webpackBootstrap
/******/ 	"use strict";
/******/ 	var __webpack_modules__ = ({

/***/ "./src/index.ts":
/*!**********************!*\
  !*** ./src/index.ts ***!
  \**********************/
/***/ (function() {


var __runInitializers = (this && this.__runInitializers) || function (thisArg, initializers, value) {
    var useValue = arguments.length > 2;
    for (var i = 0; i < initializers.length; i++) {
        value = useValue ? initializers[i].call(thisArg, value) : initializers[i].call(thisArg);
    }
    return useValue ? value : void 0;
};
var __esDecorate = (this && this.__esDecorate) || function (ctor, descriptorIn, decorators, contextIn, initializers, extraInitializers) {
    function accept(f) { if (f !== void 0 && typeof f !== "function") throw new TypeError("Function expected"); return f; }
    var kind = contextIn.kind, key = kind === "getter" ? "get" : kind === "setter" ? "set" : "value";
    var target = !descriptorIn && ctor ? contextIn["static"] ? ctor : ctor.prototype : null;
    var descriptor = descriptorIn || (target ? Object.getOwnPropertyDescriptor(target, contextIn.name) : {});
    var _, done = false;
    for (var i = decorators.length - 1; i >= 0; i--) {
        var context = {};
        for (var p in contextIn) context[p] = p === "access" ? {} : contextIn[p];
        for (var p in contextIn.access) context.access[p] = contextIn.access[p];
        context.addInitializer = function (f) { if (done) throw new TypeError("Cannot add initializers after decoration has completed"); extraInitializers.push(accept(f || null)); };
        var result = (0, decorators[i])(kind === "accessor" ? { get: descriptor.get, set: descriptor.set } : descriptor[key], context);
        if (kind === "accessor") {
            if (result === void 0) continue;
            if (result === null || typeof result !== "object") throw new TypeError("Object expected");
            if (_ = accept(result.get)) descriptor.get = _;
            if (_ = accept(result.set)) descriptor.set = _;
            if (_ = accept(result.init)) initializers.unshift(_);
        }
        else if (_ = accept(result)) {
            if (kind === "field") initializers.unshift(_);
            else descriptor[key] = _;
        }
    }
    if (target) Object.defineProperty(target, contextIn.name, descriptor);
    done = true;
};
function loggedMethod(headMessage = "LOG:") {
    return function actualDecorator(originalMethod, context) {
        const methodName = String(context.name);
        function replacementMethod(...args) {
            console.log(`${headMessage} Entering method '${methodName}'.`);
            const result = originalMethod.call(this, ...args);
            console.log(`${headMessage} Exiting method '${methodName}'.`);
            return result;
        }
        return replacementMethod;
    };
}
let ExampleClass = (() => {
    var _a;
    let _instanceExtraInitializers = [];
    let _greet_decorators;
    return _a = class ExampleClass {
            greet() {
                console.log("Hello, there");
            }
            constructor() {
                __runInitializers(this, _instanceExtraInitializers);
            }
        },
        (() => {
            const _metadata = typeof Symbol === "function" && Symbol.metadata ? Object.create(null) : void 0;
            _greet_decorators = [loggedMethod()];
            __esDecorate(_a, null, _greet_decorators, { kind: "method", name: "greet", static: false, private: false, access: { has: obj => "greet" in obj, get: obj => obj.greet }, metadata: _metadata }, null, _instanceExtraInitializers);
            if (_metadata) Object.defineProperty(_a, Symbol.metadata, { enumerable: true, configurable: true, writable: true, value: _metadata });
        })(),
        _a;
})();
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
//# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiYnVuZGxlLmpzIiwibWFwcGluZ3MiOiI7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7OztBQUFBLFNBQVMsWUFBWSxDQUFDLFdBQVcsR0FBRyxNQUFNO0lBQ3RDLE9BQU8sU0FBUyxlQUFlLENBQUMsY0FBbUIsRUFBRSxPQUFvQztRQUNyRixNQUFNLFVBQVUsR0FBRyxNQUFNLENBQUMsT0FBTyxDQUFDLElBQUksQ0FBQyxDQUFDO1FBRXhDLFNBQVMsaUJBQWlCLENBQVksR0FBRyxJQUFXO1lBQ2hELE9BQU8sQ0FBQyxHQUFHLENBQUMsR0FBRyxXQUFXLHFCQUFxQixVQUFVLElBQUksQ0FBQztZQUM5RCxNQUFNLE1BQU0sR0FBRyxjQUFjLENBQUMsSUFBSSxDQUFDLElBQUksRUFBRSxHQUFHLElBQUksQ0FBQyxDQUFDO1lBQ2xELE9BQU8sQ0FBQyxHQUFHLENBQUMsR0FBRyxXQUFXLG9CQUFvQixVQUFVLElBQUksQ0FBQztZQUM3RCxPQUFPLE1BQU0sQ0FBQztRQUNsQixDQUFDO1FBRUQsT0FBTyxpQkFBaUIsQ0FBQztJQUM3QixDQUFDO0FBQ0wsQ0FBQztJQUVLLFlBQVk7Ozs7c0JBQVosWUFBWTtZQUVoQixLQUFLO2dCQUNOLE9BQU8sQ0FBQyxHQUFHLENBQUMsY0FBZSxDQUFDLENBQUM7WUFDNUIsQ0FBQzs7Z0JBSkcsbURBQVk7Ozs7O2lDQUNmLFlBQVksRUFBRTtZQUNmLHFLQUFLLDZEQUVKOzs7OztBQUdILElBQUksWUFBWSxFQUFFLENBQUMsS0FBSyxFQUFFLENBQUM7Ozs7Ozs7O1VFdEIzQjtVQUNBO1VBQ0E7VUFDQTtVQUNBIiwic291cmNlcyI6WyJ3ZWJwYWNrOi8vZGVwLWluamVjdGlvbi8uL3NyYy9pbmRleC50cyIsIndlYnBhY2s6Ly9kZXAtaW5qZWN0aW9uL3dlYnBhY2svYmVmb3JlLXN0YXJ0dXAiLCJ3ZWJwYWNrOi8vZGVwLWluamVjdGlvbi93ZWJwYWNrL3N0YXJ0dXAiLCJ3ZWJwYWNrOi8vZGVwLWluamVjdGlvbi93ZWJwYWNrL2FmdGVyLXN0YXJ0dXAiXSwic291cmNlc0NvbnRlbnQiOlsiZnVuY3Rpb24gbG9nZ2VkTWV0aG9kKGhlYWRNZXNzYWdlID0gXCJMT0c6XCIpIHtcbiAgICByZXR1cm4gZnVuY3Rpb24gYWN0dWFsRGVjb3JhdG9yKG9yaWdpbmFsTWV0aG9kOiBhbnksIGNvbnRleHQ6IENsYXNzTWV0aG9kRGVjb3JhdG9yQ29udGV4dCkge1xuICAgICAgICBjb25zdCBtZXRob2ROYW1lID0gU3RyaW5nKGNvbnRleHQubmFtZSk7XG5cbiAgICAgICAgZnVuY3Rpb24gcmVwbGFjZW1lbnRNZXRob2QodGhpczogYW55LCAuLi5hcmdzOiBhbnlbXSkge1xuICAgICAgICAgICAgY29uc29sZS5sb2coYCR7aGVhZE1lc3NhZ2V9IEVudGVyaW5nIG1ldGhvZCAnJHttZXRob2ROYW1lfScuYClcbiAgICAgICAgICAgIGNvbnN0IHJlc3VsdCA9IG9yaWdpbmFsTWV0aG9kLmNhbGwodGhpcywgLi4uYXJncyk7XG4gICAgICAgICAgICBjb25zb2xlLmxvZyhgJHtoZWFkTWVzc2FnZX0gRXhpdGluZyBtZXRob2QgJyR7bWV0aG9kTmFtZX0nLmApXG4gICAgICAgICAgICByZXR1cm4gcmVzdWx0O1xuICAgICAgICB9XG5cbiAgICAgICAgcmV0dXJuIHJlcGxhY2VtZW50TWV0aG9kO1xuICAgIH1cbn1cbiBcbmNsYXNzIEV4YW1wbGVDbGFzcyB7XG4gIEBsb2dnZWRNZXRob2QoKVxuICBncmVldCgpIHtcblx0Y29uc29sZS5sb2coXCJIZWxsbywgdGhlcmVcIiEpO1xuICB9XG59XG5cbm5ldyBFeGFtcGxlQ2xhc3MoKS5ncmVldCgpO1xuIiwiIiwiLy8gc3RhcnR1cFxuLy8gTG9hZCBlbnRyeSBtb2R1bGUgYW5kIHJldHVybiBleHBvcnRzXG4vLyBUaGlzIGVudHJ5IG1vZHVsZSBpcyByZWZlcmVuY2VkIGJ5IG90aGVyIG1vZHVsZXMgc28gaXQgY2FuJ3QgYmUgaW5saW5lZFxudmFyIF9fd2VicGFja19leHBvcnRzX18gPSB7fTtcbl9fd2VicGFja19tb2R1bGVzX19bXCIuL3NyYy9pbmRleC50c1wiXSgpO1xuIiwiIl0sIm5hbWVzIjpbXSwic291cmNlUm9vdCI6IiJ9