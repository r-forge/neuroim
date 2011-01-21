#' @include AllGeneric.R
roxygen()
#' @include AllClass.R
roxygen()


None <- new("NamedAxis", axis="None")

NullAxis <- new("AxisSet", ndim=as.integer(0))

LEFT_RIGHT <- new("NamedAxis", axis="Left-to-Right")
RIGHT_LEFT <- new("NamedAxis", axis="Right-to-Left")
ANT_POST   <- new("NamedAxis", axis="Anterior-to-Posterior")
POST_ANT   <- new("NamedAxis", axis="Posterior-to-Anterior")
INF_SUP    <- new("NamedAxis", axis="Inferior-to-Superior")
SUP_INF    <- new("NamedAxis", axis="Superior-to-Inferior")

TIME <- new("NamedAxis", axis="Time")
TimeAxis <- new("AxisSet1D", ndim=as.integer(1), i=TIME)

AxisSet2D <- function(i, j) {
	new("AxisSet2D", ndim=as.integer(2), i=i, j=j)	
}

AxisSet3D <- function(i, j, k) {
	new("AxisSet3D", ndim=as.integer(3), i=i, j=j, k=k)	
}


#' @rdname ndim-methods
setMethod(f="ndim",signature=signature(x= "AxisSet"), def=function(x) { x@ndim })

#' @nord
setMethod(f="show", signature=signature("NamedAxis"), 
		def=function(object) {
			cat(object@axis)
		})

#' @nord
setMethod(f="print", signature=signature("NamedAxis"), 
		def=function(x, ...) {
			x@axis
		})

#' @nord
setMethod(f="show", signature=signature("AxisSet1D"), 
		def=function(object) {
			cat("instance of class:", class(object), "\n\n")
			cat("Axis 1:", print(object@i), "\n")
		})

#' @nord
setMethod(f="print", signature=signature("AxisSet2D"), 
		def=function(x, ...) {
			paste(print(x@i), "-", print(x@j))
		})

#' @nord
setMethod(f="show", signature=signature("AxisSet2D"), 
		def=function(object) {
			cat("instance of class:", class(object), "\n\n")
			cat("Axis 1:", print(object@i), "\n")
			cat("Axis 2:", print(object@j), "\n")
		})

#' #' @nord
setMethod(f="print", signature=signature("AxisSet3D"), 
		def=function(x, ...) {
			paste(print(x@i), "-", print(x@j), "-", print(x@k))
		})


#' @nord
setMethod(f="show", signature=signature("AxisSet3D"), 
		def=function(object) {
			cat("instance of class:", class(object), "\n\n")
			cat("Axis 1:", print(object@i), "\n")
			cat("Axis 2:", print(object@j), "\n")
			cat("Axis 3:", print(object@k), "\n")
		})


#' @nord
setMethod(f="show", signature=signature("AxisSet4D"), 
		def=function(object) {
			cat("instance of class:", class(object), "\n\n")
			cat("Axis 1:", print(object@i), "\n")
			cat("Axis 2:", print(object@j), "\n")
			cat("Axis 3:", print(object@k), "\n")
			cat("Axis 4:", print(object@l), "\n")
			
		})



OrientationList2D <- list(
	SAGITTAL_AI = AxisSet2D(ANT_POST, INF_SUP), 
	SAGITTAL_PI = AxisSet2D(POST_ANT, INF_SUP),
	SAGITTAL_PS = AxisSet2D(POST_ANT, SUP_INF),
	SAGITTAL_AS = AxisSet2D(ANT_POST, SUP_INF),
	SAGITTAL_IA = AxisSet2D(INF_SUP, ANT_POST),
	SAGITTAL_IP = AxisSet2D(INF_SUP, POST_ANT),
	SAGITTAL_SP = AxisSet2D(SUP_INF, POST_ANT),
	SAGITTAL_SA = AxisSet2D(SUP_INF, ANT_POST),

	CORONAL_LI = AxisSet2D(LEFT_RIGHT, INF_SUP),
	CORONAL_RI = AxisSet2D(RIGHT_LEFT, INF_SUP),
	CORONAL_RS = AxisSet2D(RIGHT_LEFT, SUP_INF),
	CORONAL_LS = AxisSet2D(LEFT_RIGHT, SUP_INF),
	CORONAL_IL = AxisSet2D(INF_SUP, LEFT_RIGHT),
	CORONAL_IR = AxisSet2D(INF_SUP, RIGHT_LEFT),
	CORONAL_SR = AxisSet2D(SUP_INF, RIGHT_LEFT),
	CORONAL_SL = AxisSet2D(SUP_INF, LEFT_RIGHT),


	AXIAL_LA = AxisSet2D(LEFT_RIGHT, ANT_POST),
	AXIAL_RA = AxisSet2D(RIGHT_LEFT, ANT_POST),
	AXIAL_RP = AxisSet2D(RIGHT_LEFT, POST_ANT),
	AXIAL_LP = AxisSet2D(LEFT_RIGHT, POST_ANT),
	AXIAL_AL = AxisSet2D(ANT_POST, LEFT_RIGHT),
	AXIAL_AR = AxisSet2D(ANT_POST, RIGHT_LEFT),
	AXIAL_PL = AxisSet2D(POST_ANT, LEFT_RIGHT),
	AXIAL_PR = AxisSet2D(POST_ANT, RIGHT_LEFT))

OrientationList3D <- list(
	SAGITTAL_AIL = AxisSet3D(ANT_POST, INF_SUP, LEFT_RIGHT), 
	SAGITTAL_PIL = AxisSet3D(POST_ANT, INF_SUP, LEFT_RIGHT),
	SAGITTAL_PSL = AxisSet3D(POST_ANT, SUP_INF, LEFT_RIGHT),
	SAGITTAL_ASL = AxisSet3D(ANT_POST, SUP_INF, LEFT_RIGHT),
	SAGITTAL_IAL = AxisSet3D(INF_SUP,  ANT_POST, LEFT_RIGHT),
	SAGITTAL_IPL = AxisSet3D(INF_SUP,  POST_ANT, LEFT_RIGHT),
	SAGITTAL_SPL = AxisSet3D(SUP_INF,  POST_ANT, LEFT_RIGHT),
	SAGITTAL_SAL = AxisSet3D(SUP_INF,  ANT_POST, LEFT_RIGHT),

	SAGITTAL_AIR = AxisSet3D(ANT_POST, INF_SUP, RIGHT_LEFT), 
	SAGITTAL_PIR = AxisSet3D(POST_ANT, INF_SUP, RIGHT_LEFT),
	SAGITTAL_PSR = AxisSet3D(POST_ANT, SUP_INF, RIGHT_LEFT),
	SAGITTAL_ASR = AxisSet3D(ANT_POST, SUP_INF, RIGHT_LEFT),
	SAGITTAL_IAR = AxisSet3D(INF_SUP,  ANT_POST, RIGHT_LEFT),
	SAGITTAL_IPR = AxisSet3D(INF_SUP,  POST_ANT, RIGHT_LEFT),
	SAGITTAL_SPR = AxisSet3D(SUP_INF,  POST_ANT, RIGHT_LEFT),
	SAGITTAL_SAR = AxisSet3D(SUP_INF,  ANT_POST, RIGHT_LEFT),

	CORONAL_LIA = AxisSet3D(LEFT_RIGHT, INF_SUP, ANT_POST),
	CORONAL_RIA = AxisSet3D(RIGHT_LEFT, INF_SUP, ANT_POST),
	CORONAL_RSA = AxisSet3D(RIGHT_LEFT, SUP_INF, ANT_POST),
	CORONAL_LSA = AxisSet3D(LEFT_RIGHT, SUP_INF, ANT_POST),
	CORONAL_ILA = AxisSet3D(INF_SUP,    LEFT_RIGHT, ANT_POST),
	CORONAL_IRA = AxisSet3D(INF_SUP,    RIGHT_LEFT, ANT_POST),
	CORONAL_SRA = AxisSet3D(SUP_INF,    RIGHT_LEFT, ANT_POST),
	CORONAL_SLA = AxisSet3D(SUP_INF,    LEFT_RIGHT, ANT_POST),

	CORONAL_LIP = AxisSet3D(LEFT_RIGHT, INF_SUP, POST_ANT),
	CORONAL_RIP = AxisSet3D(RIGHT_LEFT, INF_SUP, POST_ANT),
	CORONAL_RSP = AxisSet3D(RIGHT_LEFT, SUP_INF, POST_ANT),
	CORONAL_LSP = AxisSet3D(LEFT_RIGHT, SUP_INF, POST_ANT),
	CORONAL_ILP = AxisSet3D(INF_SUP,    LEFT_RIGHT, POST_ANT),
	CORONAL_IRP = AxisSet3D(INF_SUP,    RIGHT_LEFT, POST_ANT),
	CORONAL_SRP = AxisSet3D(SUP_INF,    RIGHT_LEFT, POST_ANT),
	CORONAL_SLP = AxisSet3D(SUP_INF,    LEFT_RIGHT, POST_ANT),


	AXIAL_LAI = AxisSet3D(LEFT_RIGHT, ANT_POST, INF_SUP),
	AXIAL_RAI = AxisSet3D(RIGHT_LEFT, ANT_POST, INF_SUP),
	AXIAL_RPI = AxisSet3D(RIGHT_LEFT, POST_ANT, INF_SUP),
	AXIAL_LPI = AxisSet3D(LEFT_RIGHT, POST_ANT, INF_SUP),
	AXIAL_ALI = AxisSet3D(ANT_POST,   LEFT_RIGHT, INF_SUP),
	AXIAL_ARI = AxisSet3D(ANT_POST,   RIGHT_LEFT, INF_SUP),
	AXIAL_PLI = AxisSet3D(POST_ANT,   LEFT_RIGHT, INF_SUP),
	AXIAL_PRI = AxisSet3D(POST_ANT,   RIGHT_LEFT, INF_SUP),

	AXIAL_LAS = AxisSet3D(LEFT_RIGHT, ANT_POST, SUP_INF),
	AXIAL_RAS = AxisSet3D(RIGHT_LEFT, ANT_POST, SUP_INF),
	AXIAL_RPS = AxisSet3D(RIGHT_LEFT, POST_ANT, SUP_INF),
	AXIAL_LPS = AxisSet3D(LEFT_RIGHT, POST_ANT, SUP_INF),
	AXIAL_ALS = AxisSet3D(ANT_POST,   LEFT_RIGHT, SUP_INF),
	AXIAL_ARS = AxisSet3D(ANT_POST,   RIGHT_LEFT, SUP_INF),
	AXIAL_PLS = AxisSet3D(POST_ANT,   LEFT_RIGHT, SUP_INF),
	AXIAL_PRS = AxisSet3D(POST_ANT,   RIGHT_LEFT, SUP_INF))
	
	
	
#' given three named axes return AxisSet3D singleton
#' @param axis1 the first axis
#' @param axis2 the second axis
#' @param axis3 the thrid axis	
#' @export matchAnatomy3D 
matchAnatomy3D <- function(axis1, axis2, axis3) {
	for (orient in OrientationList3D) {
		if (identical(orient@i,axis1) && identical(orient@j,axis2) && identical(orient@k, axis3)) {
			return(orient)
		}
	}
	
	stop("did not find legal matching anatomical orientation for axes: ",
			axis1, axis2, axis3)
		
}

#' given two named axes return AxisSet2D singleton
#' @param axis1 the first axis
#' @param axis2 the second axis
#' @export matchAnatomy2D 
matchAnatomy2D <- function(axis1, axis2) {
	for (orient in OrientationList2D) {
		if (identical(orient@i,axis1) && identical(orient@j,axis2)) {
			return(orient)
		}
	}
	
	stop("did not find legal matching anatomical orientation for axes: ",
			axis1, axis2)
	
}

