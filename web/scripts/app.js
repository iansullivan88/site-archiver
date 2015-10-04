(function() {

  var app = angular.module('siteArchiver', ['siteArchiver.controllers', 'siteArchiver.providers', 'ngRoute']);

  app.config(['$routeProvider', '$locationProvider', function($routeProvider) {

    $routeProvider
      .when('/', {
        templateUrl: 'partials/home.html',
        controller: 'HomeCtrl'
      })
      .when('/site/:siteId', {
        templateUrl: 'partials/site.html',
        controller: 'SiteCtrl'
      })
      .otherwise({
        redirectTo: '/'
      });

  }]);

})();

angular.module('siteArchiver.controllers', []);
angular.module('siteArchiver.providers', ['ngResource']);
