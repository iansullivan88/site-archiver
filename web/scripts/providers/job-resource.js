angular.module('siteArchiver.providers').factory('Job', ['$resource', function($resource) {
  return $resource('/resources/job/:siteId');
}]);
