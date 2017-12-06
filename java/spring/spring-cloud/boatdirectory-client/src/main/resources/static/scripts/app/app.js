/**
 * 
 */

var app = angular.module('MyApp', []);

app.controller('MyCtrl', function($scope, $http, $location) {
	
	$scope.boatNames = [];
	
	$scope.request = function() {
		$scope.error = "";
		baseUrl = $location.protocol() + '://'+ $location.host() +':'+  $location.port();
		$http.get(baseUrl + '/rest/boats/names')
			.then(function(response) {
				$scope.boatNames = response.data;
			}, function(reason) {
				$scope.error = "Could not get boat names: " + reason;
			}, function(value) {
				$scope.error = value;
			});
	}
});
