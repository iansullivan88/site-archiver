var gulp = require('gulp'),
    concat = require('gulp-concat'),
    install = require('gulp-install'),
    sass = require('gulp-sass'),
    sourcemaps = require('gulp-sourcemaps'),
    gulpMerge = require('gulp-merge');

var outputDirectory = "../public",
    scriptsDirectory = outputDirectory,
    cssDirectory = outputDirectory;



// Dependencies
gulp.task('install', function() {
    return gulp.src(['./bower.json'])
        .pipe(install());
});

// Files relating to app itself

gulp.task('static', function() {
    return gulp.src(['static/**/*'], {base: 'static/'})
        .pipe(gulp.dest(outputDirectory));
});

gulp.task('scripts', function() {
    return gulp.src([
            'bower_components/angular/angular.js',
            'bower_components/angular-route/angular-route.js',
            'bower_components/angular-resource/angular-resource.js',
            'scripts/app.js',
            'scripts/**/*.js'])
        .pipe(sourcemaps.init())
        .pipe(concat('all.js'))
        .pipe(sourcemaps.write())
        .pipe(gulp.dest(scriptsDirectory));
});

gulp.task('styles', function() {
    var sassStream = gulp.src('./styles/*.scss')
        .pipe(sass({
            includePaths: ['bower_components/foundation/scss']
        }));

    var cssStream = gulp.src([

    ]);

    return gulpMerge(sassStream, cssStream)
        .pipe(concat('all.css'))
        .pipe(gulp.dest(cssDirectory))
})

gulp.task('watch', function() {
    gulp.watch('scripts/**/*', ['scripts']);
    gulp.watch('styles/**/*', ['styles']);
    gulp.watch('static/**/*', ['static']);
});



gulp.task('default', ['install', 'static', 'styles', 'scripts']);
