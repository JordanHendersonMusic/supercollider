RuntimeOptions {
	*scsynthPath { ^RuntimeOptions.prGet(\scsynthPath); }
	*supernovaPath { ^RuntimeOptions.prGet(\supernovaPath); }
	*prGet { |key| _GetRuntimeOptions; ^this.primitiveFailed }
}
