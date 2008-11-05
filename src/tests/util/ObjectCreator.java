/* 
 * Kodkod -- Copyright (c) 2005-2008, Emina Torlak
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package tests.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Utility methods for creating objects via constructor or method calls.
 * @author Emina Torlak
 */
public final class ObjectCreator {
	private ObjectCreator() {}
	private static Matcher name = Pattern.compile("(.+?)\\(").matcher("");
	private static Matcher arg = Pattern.compile("[\\(,]\\s*(.+?)\\s*[\\),]").matcher("");
	
	/**
	 * Returns the class with the given name
	 * @throws IllegalArgumentException - no such class exists
	 * @return class with the given name
	 */
	public static Class<?> findClass(String className) { 
		try {
			return Class.forName(className);
		} catch (ClassNotFoundException e) {
			throw new IllegalArgumentException(e);
		}
	}
	
	/**
	 * Returns all arguments in the given call, in the order in which they appear.
	 * @return all arguments in the given call, in the order in which they appear.
	 */
	private static String[] args(String call) { 
		final List<String> args = new ArrayList<String>();
		arg.reset(call);
		if (arg.find()) { 
			args.add(arg.group(1));
			while(arg.find(arg.end()-1)) { 
				args.add(arg.group(1));
			}
		}
		return args.toArray(new String[args.size()]);
	}
	
	/**
	 * Converts the given argument, represented as a string, to the given type.
	 * @throws RuntimeException - the conversion is not possible.
	 * @return the given argument converted to the given type
	 */
	@SuppressWarnings("unchecked")
	private static Object convert(String arg, Class<?> type) { 
		try {
			if (type.isPrimitive()) { 
				if (type==int.class) { 
					return new Integer(arg);
				} else if (type==long.class) { 
					return new Long(arg);
				} else if (type==boolean.class) { 
					return new Boolean(arg);
				} else if (type==double.class) { 
					return new Double(arg);
				} else if (type==float.class) { 
					return new Float(arg);
				} else if (type==byte.class) { 
					return new Byte(arg);
				} else if (type==short.class) { 
					return new Short(arg);
				} else if (type==char.class && arg.length()==1) { 
					return new Character(arg.charAt(0));
				} else {
					throw new IllegalArgumentException("Unknown primitive type: " + type);
				}
			} else if (type==String.class) { 
				return arg;
			} else if (type.isEnum()) { 
				return Enum.valueOf((Class)type, arg);
			} else if (Number.class.isAssignableFrom(type) || type==Boolean.class) { 
				final Constructor<?> c = type.getConstructor(String.class);
				return c.newInstance(arg);
			} else if (type==Character.class && arg.length()==1) { 
				return new Character(arg.charAt(0));
			} 
		} 
		catch (NoSuchMethodException e) { } 
		catch (IllegalAccessException e) { } 
		catch (InstantiationException e) { } 
		catch (InvocationTargetException e) { }
		
		throw new IllegalArgumentException();
	}
	
	/**
	 * Converts the given arguments, represented as strings, to appropriate types, if possible.
	 * If not returns null.
	 * @requires args.length = argTypes.length
	 * @return an array containing the given arguments, converted to appropriate types, if possible;
	 * null if not.
	 */
	private static Object[] convert(String[] args, Class<?>[] argTypes) { 
		assert args.length==argTypes.length;
		final Object[] out = new Object[args.length];
		for(int i = 0; i < args.length; i++) { 
			try {
				out[i] = convert(args[i], argTypes[i]);
			} catch(RuntimeException e) { return null; }
		}
		return out;
	}
	
	/**
	 * Reflectively performs the given invocation and returns the result.
	 * @requires call is of the form "name(arg1, arg2, ..., argn)" where name is the 
	 * full class name of the class whose constructor is to be invoked (e.g. java.lang.Integer) and
	 * the arguments are either strings, primitives, objects corresponding to primitives (Integer, Long, etc),
	 * or publicly accessible Enums.
	 * @throws IllegalArgumentException - call is not formatted as specified above, or it could not be made for some reason.
	 * @return an object of type T, created reflectively from the given call String
	 */
	@SuppressWarnings("unchecked")
	public static <T> T construct(String call) { 
		try {
			name.reset(call);
			if (!name.find()) throw new IllegalArgumentException("Badly formatted call: " + call);
			final Class<?> callClass = findClass(name.group(1));	
				
			final String[] args = args(call);
			
			for(Constructor<?> c : callClass.getConstructors()) { 
				final Class<?>[] argTypes = c.getParameterTypes();
				if (argTypes.length==args.length) { 
					final Object[] convertedArgs = convert(args, argTypes);
					if (convertedArgs!=null) { 
						return (T) c.newInstance(convertedArgs);
					}
				}
			}
			
			throw new IllegalAccessException("Could not call " +  call);
		} catch (InstantiationException e) {
			throw new IllegalArgumentException(e);
		} catch (IllegalAccessException e) {
			throw new IllegalArgumentException(e);
		} catch (InvocationTargetException e) { 
			throw new IllegalArgumentException(e);
		}
	}
	
	/**
	 * Reflectively performs the specified invocation  and returns the result.
	 * @requires call is of the form "name(arg1, arg2, ..., argn)" where name is the 
	 * full name of the static method to be invoked (e.g. java.lang.Integer#parseInt) and
	 * the arguments are either strings, primitives, objects corresponding to primitives (Integer, Long, etc),
	 * or publicly accessible Enums.
	 * @throws IllegalArgumentException - call is not formatted as specified above, or it could not be made for some reason.
	 * @return an object of type T, created reflectively from executing the given call String 
	 */
	@SuppressWarnings("unchecked")
	public static <T> T create(String call) { 
		try {
			name.reset(call);
			if (!name.find()) throw new IllegalArgumentException("Badly formatted call: " + call);
			final String[] parts = name.group(1).split("#");
			if (parts.length!=2) throw new IllegalArgumentException("Badly formatted call: " + call);
			final Class<?> callClass = findClass(parts[0]);
				
			final String[] args = args(call);
			
			for(Method m : callClass.getDeclaredMethods()) { 
				final int mod = m.getModifiers();
				if (!(parts[1].equals(m.getName()) && Modifier.isPublic(mod) && Modifier.isStatic(mod))) continue;
				final Class<?>[] argTypes = m.getParameterTypes();
				if (argTypes.length==args.length) { 
					final Object[] convertedArgs = convert(args, argTypes);
					if (convertedArgs!=null) { 
						return (T) m.invoke(null, convertedArgs);
					}
				}
			}
			
			throw new IllegalAccessException("Could not call " +  call);
		} catch (IllegalAccessException e) {
			throw new IllegalArgumentException(e);
		} catch (InvocationTargetException e) { 
			throw new IllegalArgumentException(e);
		}
	}
	
	/**
	 * Reflectively performs the specified invocation on the specified instance and returns the result.
	 * @requires call is of the form "name(arg1, arg2, ..., argn)" where name is the 
	 * name of the method to be invoked on the specified object and
	 * the arguments are either strings, primitives, objects corresponding to primitives (Integer, Long, etc),
	 * or publicly accessible Enums.
	 * @throws IllegalArgumentException - call is not formatted as specified above, or it could not be made for some reason.
	 * @return an object of type T, created reflectively from executing the given call String on the given instance
	 */
	@SuppressWarnings("unchecked")
	public static <T> T create(Object instance, String call) { 
		try {
			name.reset(call);
			if (!name.find()) throw new IllegalArgumentException("Badly formatted call: " + call);
			final String method = name.group(1);
			final Class<?> callClass = instance.getClass();

			final String[] args = args(call);
			
			for(Method m : callClass.getMethods()) { 
				
				if (!method.equals(m.getName())) continue;
				final Class<?>[] argTypes = m.getParameterTypes();
				if (argTypes.length==args.length) { 
 					final Object[] convertedArgs = convert(args, argTypes);
					if (convertedArgs!=null) { 
						return (T) m.invoke(instance, convertedArgs);
					}
				}
			}
			
			throw new IllegalAccessException("Could not call " +  call);
		} catch (IllegalAccessException e) {
			throw new IllegalArgumentException(e);
		} catch (InvocationTargetException e) { 
			throw new IllegalArgumentException(e);
		}
	}
	
}
