angular.module('siteArchiver.providers').factory('Site', ['$resource', function($resource) {
  return $resource('/resources/site/:id');
}]);
