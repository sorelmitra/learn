/**
 * 
 */

var app = angular.module('MyApp', []);

app.controller('MyCtrl', function($scope, $http) {
	
	$scope.boatNames = [];
	
	$scope.request = function() {
		$scope.error = "";
		$http.get('http://localhost:8080/rest/boats/names')
			.then(function(response) {
				$scope.boatNames = response.data;
			}, function(reason) {
				$scope.error = "Could not get boat names: " + reason;
			}, function(value) {
				$scope.error = value;
			});
	}
});
