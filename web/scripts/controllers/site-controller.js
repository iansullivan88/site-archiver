angular.module('siteArchiver.controllers').controller('SiteCtrl',
 ['$scope', '$routeParams', 'Site', 'Job', 'UrlHelper', function($scope, $routeParams, Site, Job, UrlHelper) {

  Site.get({id:$routeParams.siteId}, function(site) {
    $scope.site = site;
    refreshJobs(site.id);
  });

  $scope.startJob = function(siteId) {
    $scope.runningJob = true;
    var job = new Job();
    job.$save({siteId:siteId}).finally(function() {
      $scope.runningJob = false;
      refreshJobs(siteId);
    });
  }

  $scope.getJobStartUrl = function(jobId, startUrl) {
    return "/job/" + jobId + UrlHelper.getPath(startUrl);
  }

  $scope.formatTimestamp = function(timestamp) {
    var date = new Date(1000 * timestamp);
    return date.toLocaleDateString() + " " + date.toLocaleTimeString();
  }

  function refreshJobs(siteId) {
    Job.query({siteId:siteId}).$promise.then(function(jobs) {
      $scope.jobs = jobs;
    });
  }

}]);
