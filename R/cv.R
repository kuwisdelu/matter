
#### Perform cross-validation ####
## -------------------------------

cv_do <- function(fit., x, y, folds, ...,
	predict. = predict, transpose = FALSE, keep.models = TRUE,
	trainProcess = NULL, trainArgs = list(),
	testProcess = NULL, testArgs = list(),
	verbose = NA, nchunks = NA, BPPARAM = bpparam())
{
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	if ( is.na(nchunks) )
		nchunks <- getOption("matter.default.nchunks")
	if ( !is.null(dim(y)) )
		stop("response 'y' must be a vector")
	folds <- as.factor(folds)
	if ( nlevels(folds) < 2L )
		stop("need at least 2 folds")
	scores <- vector("list", length=nlevels(folds))
	models <- vector("list", length=nlevels(folds))
	names(scores) <- levels(folds)
	names(models) <- levels(folds)
	for ( i in seq_along(levels(folds)) )
	{
		fold <- levels(folds)[i]
		if ( verbose )
			message("## processing fold ", i, "/", nlevels(folds), " ",
				"(", sQuote(fold), ")")
		# create train / test sets
		test <- folds %in% fold
		train <- !test
		n_train <- sum(train)
		n_test <- sum(test)
		if ( transpose ) {
			x_train <- x[,train,drop=FALSE]
			x_test <- x[,test,drop=FALSE]
		} else {
			x_train <- x[train,,drop=FALSE]
			x_test <- x[test,,drop=FALSE]
		}
		y_train <- y[train]
		y_test <- y[test]
		# preprocess training data
		if ( is.function(trainProcess) ) {
			if ( verbose )
				message("# pre-processing training data")
			args <- c(trainArgs, list(nchunks=nchunks, BPPARAM=BPPARAM))
			args <- c(list(x_train), args)
			x_train <- do.call(trainProcess, args)
		}
		# train model
		if ( verbose )
			message("# fitting model on pooled training sets (n=", n_train, ")")
		fit <- fit.(x_train, y_train,
			nchunks=nchunks, BPPARAM=BPPARAM, ...)
		if ( keep.models )
			models[[i]] <- fit
		# preprocess test data
		if ( is.function(testProcess) ) {
			if ( verbose )
				message("# pre-processing test data")
			args <- c(testArgs, list(nchunks=nchunks, BPPARAM=BPPARAM))
			args <- c(list(x_test, x_train), args)
			x_train <- do.call(testProcess, args)
		}
		# predict
		if ( verbose )
			message("# evaluating model on test set (n=", n_test, ")")
		y_pred <- predict.(fit, x_test,
			nchunks=nchunks, BPPARAM=BPPARAM, ...)
		# predict classes if classification
		if ( is_discrete(y) && !is_discrete(y_pred) )
		{
			if ( is.array(y_pred) ) {
				margin <- length(dim(y_pred))
				y_pred <- apply(y_pred, margin, predict_class, simplify=FALSE)
				y_pred <- as.data.frame(y_pred, check.names=FALSE)
			} else {
				y_pred <- predict_class(y_pred)
			}
		}
		# evaluate
		if ( is.array(y_pred) ) {
			margin <- length(dim(y_pred))
			sc <- apply(y_pred, margin, predscore_macro, y_test)
			sc <- t(sc)
		} else if ( is.data.frame(y_pred) || is.list(y_pred) ) {
			sc <- do.call(rbind, lapply(y_pred, predscore_macro, y_test))
		} else {
			sc <- predscore_macro(y_pred, y_test)
		}
		scores[[i]] <- sc
	}
	if ( verbose )
		message("## summarizing ", nlevels(folds), " folds")
	average <- Reduce(`+`, scores) / length(scores)
	ans <- list(average=average, scores=scores)
	if ( keep.models )
		ans$models <- models
	structure(ans, class="cv")
}

print.cv <- function(x, ...)
{
	cat(sprintf("Cross validation with %d folds\n", length(x$scores)))
	cat("\nAverage accuracy:\n")
	print(x$average, ...)
	invisible(x)
}

# score predictions from classification or regression
predscore <- function(x, ref)
{
	if ( length(x) != length(ref) )
		stop("'x' and 'ref' must have the same length")
	nas <- is.na(x) | is.na(ref)
	x <- x[!nas]
	ref <- ref[!nas]
	if ( is_discrete(x) && is_discrete(ref) ) {
		y <- as.factor(ref)
		if ( !is.factor(x) || !setequal(levels(x), levels(y)) )
			x <- factor(x, levels=levels(y))
		t(vapply(levels(y),
			function(lvl) {
				correct <- x == y
				c(Recall=mean(correct[y == lvl]),
					Precision=mean(correct[x == lvl]))
			}, numeric(2L)))
	} else if ( is.numeric(x) && is.numeric(x) ) {
		c(RMSE=sqrt(mean((ref - x)^2)),
			MAE=mean(abs(ref - x)),
			MAPE=mean(abs((ref - x) / ref)))
	} else {
		stop("'x' and 'ref' must be both discrete or both numeric")
	}
}

# score average performance over classes
predscore_macro <- function(x, y)
{
	scores <- predscore(x, y)
	if ( is.matrix(scores) )
	{
		scores <- colMeans(scores)
		metrics <- paste0("Macro", names(scores))
		names(scores) <- metrics
	}
	scores
}

