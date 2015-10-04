angular.module('siteArchiver.providers').factory('UrlHelper', [function() {

  var a = document.createElement('a');

  return {
    getPath: function(url) {
      a.href = url;
      var path = a.pathname;

      // If the path is '/' then the url could be http://domain.com/ or http://domain.com
      // I am treating them differently so need to manually handle empty paths
      if (path == '/' && url.substr(url.length - 1) != '/') {
        return '';
      }

      return path;
    }
  }

}]);
