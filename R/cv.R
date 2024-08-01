
#### Perform cross-validation ####
## -------------------------------

cv_do <- function(fit., x, y, folds, ...,
	mi = !is.null(bags), bags = NULL, pos = 1L,
	predict. = predict, transpose = FALSE, keep.models = TRUE,
	trainProcess = NULL, trainArgs = list(),
	testProcess = NULL, testArgs = list(),
	verbose = NA, chunkopts = list(),
	BPPARAM = bpparam())
{
	if ( is.na(verbose) )
		verbose <- getOption("matter.default.verbose")
	if ( NCOL(y) > 1L )
		matter_error("matrix response 'y' not supported")
	folds <- as.factor(folds)
	if ( nlevels(folds) < 2L )
		matter_error("need at least 2 folds")
	if ( mi && is.null(bags) )
		matter_error("missing 'bags' for multiple instance learning")
	if ( !is.null(bags) )
	{
		y <- as.factor(y)
		if ( nlevels(y) != 2L )
			matter_error("y must have exactly 2 levels")
		if ( is.integer(pos) )
			pos <- levels(y)[pos]
		neg <- setdiff(levels(y), pos)
		if ( verbose )
			message("## using multiple instance learning ",
				"with ", nlevels(bags), " bags")
		y_bags <- vapply(levels(bags),
			function(bag) {
				yi <- y[!is.na(y) & bags %in% bag]
				if ( any(yi %in% pos) ) {
					pos
				} else {
					neg
				}
			}, character(1L))
		y_inst <- y_bags[as.integer(bags)]
		y_inst <- replace(y_inst, is.na(y), NA)
		y_inst <- factor(y_inst, levels=levels(y))
	}
	scores <- vector("list", length=nlevels(folds))
	models <- vector("list", length=nlevels(folds))
	y_out <- vector("list", length=nlevels(folds))
	names(scores) <- levels(folds)
	names(models) <- levels(folds)
	names(y_out) <- levels(folds)
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
			args <- c(trainArgs, list(chunkopts=chunkopts, BPPARAM=BPPARAM))
			args <- c(list(x_train), args)
			x_train <- do.call(trainProcess, args)
		}
		# train model
		if ( verbose )
			message("# fitting model on pooled training sets (n=", n_train, ")")
		if ( is.null(bags) ) {
			fit <- fit.(x_train, y_train,
				chunkopts=chunkopts, BPPARAM=BPPARAM, ...)
		} else {
			if ( mi ) {
				fit <- mi_learn(fit., x_train, y_inst[train],
					bags=bags[train], pos=pos, verbose=verbose,
					chunkopts=chunkopts, BPPARAM=BPPARAM, ...)
			} else {
				fit <- fit.(x_train, y_inst[train],
					bags=bags[train], pos=pos, verbose=verbose,
					chunkopts=chunkopts, BPPARAM=BPPARAM, ...)
			}
		}
		if ( keep.models )
			models[[i]] <- fit
		# preprocess test data
		if ( is.function(testProcess) ) {
			if ( verbose )
				message("# pre-processing test data")
			args <- c(testArgs, list(chunkopts=chunkopts, BPPARAM=BPPARAM))
			args <- c(list(x_test, x_train), args)
			x_test <- do.call(testProcess, args)
		}
		# predict
		if ( verbose )
			message("# evaluating model on test set (n=", n_test, ")")
		y_pred <- predict.(fit, x_test,
			chunkopts=chunkopts, BPPARAM=BPPARAM, ...)
		y_out[[i]] <- y_pred
		# predict classes if classification
		if ( is_discrete(y) && !is_discrete(y_pred) )
		{
			if ( !is.matrix(y_pred) ) {
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
			sc <- apply(y_pred, margin, predscore, y_test)
		} else if ( is.data.frame(y_pred) || is.list(y_pred) ) {
			sc <- lapply(y_pred, predscore, y_test)
		} else {
			sc <- predscore(y_pred, y_test)
		}
		scores[[i]] <- simplify2array(sc)
	}
	# compile cv predictions
	y_dim <- c(length(y), dim(y_out[[1L]])[-1L])
	y_dimn <- c(list(names(y)), dimnames(y_out[[1L]])[-1L])
	fitted.values <- array(NA_real_, dim=y_dim, dimnames=y_dimn)
	for ( i in seq_along(levels(folds)) )
	{
		fold <- levels(folds)[i]
		if ( length(y_dim) == 1L ) {
			fitted.values[folds %in% fold] <- y_out[[i]]
		} else if ( length(y_dim) == 2L ) {
			fitted.values[folds %in% fold,] <- y_out[[i]]
		} else if ( length(y_dim) == 3L ) {
			fitted.values[folds %in% fold,,] <- y_out[[i]]
		} else {
			matter_error("fitted values with more than 3 dimensions not supported")
		}
	}
	fitted.values <- array2list(fitted.values, length(y_dim))
	# average and return results
	if ( verbose )
		message("## summarizing ", nlevels(folds), " folds")
	FUN <- function(sc) {
		if ( isTRUE(length(dim(sc)) > 2L) ) {
			sc <- colMeans(sc)
			if ( is_discrete(y) )
				rownames(sc) <- paste0("Macro", rownames(sc))
		}
		t(sc)
	}
	macros <- lapply(scores, FUN)
	average <- Reduce(`+`, macros) / length(macros)
	ans <- list(average=average, scores=scores, folds=folds)
	if ( keep.models )
		ans$models <- models
	ans$fitted.values <- fitted.values
	structure(ans, class="cv")
}

fitted.cv <- function(object, type = c("response", "class"),
	simplify = TRUE, ...)
{
	type <- match.arg(type)
	y <- object$fitted.values
	if ( type == "class" ) {
		y <- simplify2array(y)
		if ( !is.matrix(y) ) {
			margin <- length(dim(y))
			y <- apply(y, margin, predict_class, simplify=FALSE)
			if ( simplify )
				y <- as.data.frame(y, check.names=FALSE)
		} else {
			y <- predict_class(y)
		}
	} else if ( simplify ) {
		y <- simplify2array(y)
	}
	y
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
		matter_error("'x' and 'ref' must have the same length")
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
		matter_error("'x' and 'ref' must be both discrete or both numeric")
	}
}

# calculate AUC from ROC curve
rocscore <- function(x, ref, n = 32L)
{
	if ( length(x) != length(ref) )
		matter_error("'x' and 'ref' must have the same length")
	if ( !is.numeric(x) )
		matter_error("'x' must be numeric")
	if ( !is.logical(ref) )
		matter_error("'ref' must be logical")
	nas <- is.na(x) | is.na(ref)
	x <- x[!nas]
	ref <- ref[!nas]
	threshold <- seq(min(x), max(x), length.out=n)
	roc <- vapply(threshold,
		function(t) {
			pos <- x > t
			TPR <- mean(pos[ref])
			FPR <- mean(pos[!ref])
			c(TPR=TPR, FPR=FPR)
		}, numeric(2L))
	roc <- data.frame(threshold=threshold,
		TPR=roc["TPR",], FPR=roc["FPR",])
	auc <- abs(sum(roc$TPR[-1] * diff(roc$FPR)))
	structure(auc, ROC=roc)
}

