package sorel.android;

import android.opengl.GLES20;

public class ShaderUtils {

	public static int loadShader(int type, String shaderCode){
	    int shader = GLES20.glCreateShader(type);

	    // add the source code to the shader and compile it
	    GLES20.glShaderSource(shader, shaderCode);
	    GLES20.glCompileShader(shader);

	    return shader;
	}
	
}
