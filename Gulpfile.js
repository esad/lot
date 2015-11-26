var gulp = require('gulp');
var elm  = require('gulp-elm');
var sass = require('gulp-sass');
var path = require('path');

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
  return gulp.src("src/*.js").pipe(gulp.dest(dest))
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

gulp.task('default', ['watch','elm', 'sass', 'copy:index', 'copy:js']);