var gulp = require('gulp');
var elm  = require('gulp-elm');
var sass = require('gulp-sass');
var path = require('path');
var gutil = require('gulp-util');
var ftp = require('vinyl-ftp');
var webserver = require('gulp-webserver');

require('dotenv').load();

var dest = path.resolve('./build');

gulp.task('elm-init', elm.init);

gulp.task('elm', ['elm-init'], function(){
  // process.chdir('src/elm');
  return gulp.src('src/elm/App.elm').pipe(elm()).pipe(gulp.dest(dest));
});

gulp.task("copy:index", function () {
  return gulp.src("src/index.html").pipe(gulp.dest(dest))
});

gulp.task("copy:js", function() {
  return gulp.src("src/**/*.js").pipe(gulp.dest(dest))
})

gulp.task("copy:vendor", function() {
  return gulp.src("vendor/**/*").pipe(gulp.dest(dest))
})

gulp.task('sass', function () {
  gulp.src('src/*.scss')
    .pipe(sass().on('error', sass.logError))
    .pipe(gulp.dest(dest));
});

gulp.task('watch', function() {
  gulp.watch('src/elm/*.elm', ['elm']);
  gulp.watch('src/*.scss', ['sass']);
});

gulp.task('build', ['elm', 'sass', 'copy:index', 'copy:js', 'copy:vendor']);

gulp.task('deploy', ['build'], function() {
  var conn = ftp.create({
    host: process.env.DEPLOYMENT_FTP_HOST,
    user: process.env.DEPLOYMENT_FTP_USERNAME,
    password: process.env.DEPLOYMENT_FTP_PASSWORD,
    log: gutil.log
  });

  return gulp.src(dest + '/**',{base:dest}).pipe(conn.dest( process.env.DEPLOYMENT_FTP_REMOTE_DIR));
});

gulp.task('serve', function() {
  gulp.src(dest).pipe(webserver({
    directoryListing: false,
    open: true
  }));
});

gulp.task('default', ['watch','build','serve']);