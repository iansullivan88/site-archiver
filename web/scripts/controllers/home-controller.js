angular.module('siteArchiver.controllers').controller('HomeCtrl', ['$scope', 'Site', function($scope, Site) {

  $scope.newSite = {};

  $scope.createNewSite = function(formValues) {
    if (!$scope.newSiteForm.$valid) {
      return;
    }
    var newSite = new Site(formValues);
    newSite.$save().finally(function() {
      refreshSites();
    });
  }

  function refreshSites() {
    Site.query().$promise.then(function(sites) {
      $scope.sites = sites;
    });
  }

  refreshSites();



}]);
