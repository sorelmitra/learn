using Microsoft.AspNet.Builder;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;

// Namespace of the app
namespace FirstMacAspNet5App.Controllers.Web
{
    public class Startup
    {
        // Internal property; apparently it doesn't need storage
        IConfigurationRoot Configuration { get; set; }

        // Constructor; You can add parameters of types to be injected by MVC
        public Startup()
        {
            // appsettings.json can be used for configurations
            var configBuilder = new ConfigurationBuilder()
                .AddJsonFile("appsettings.json");
            // Set the property to what's returned by configBuilder
            // Apparently you don't need do declare storag, just the property with
            // { get; set; } above
            Configuration = configBuilder.Build();
        }
        
        // This method gets called by the runtime. Use this method to add services to the container.
        // For more information on how to configure your application, visit http://go.microsoft.com/fwlink/?LinkID=398940
        public void ConfigureServices(IServiceCollection services)
        {
            // Add a known service named MVC. I this case, MVC comes from Microsoft (you need to set its dependency up in project.json), but the same pattern is used for services you implement?
            // You need to "use" the service in Configure in order to be able to actually serve in the MVC style
            services.AddMvc();
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        // This means that here we define how does the app handle HTTP requests
        public void Configure(IApplicationBuilder app, ILoggerFactory loggerFactory)
        {
            // Order of calls in this function DOES matter!
            
            loggerFactory.MinimumLevel = LogLevel.Debug;
            
            // This is the web server itself; you probably want it here if
            // you're using IIS
            app.UseIISPlatformHandler();

            // ASP.NET 5 is configurable from scratch. You can choose what services to use
            // in your app.
            // The code below was originally created by "yo aspnet" and generates a plain
            // text response.
            // We will be using Static files and MVC, so we commented it out
            /*
            app.Run(async (context) =>
            {
                await context.Response.WriteAsync("Hello World!");
            });
            */
            
            // Enable static files in this app. They can be accessed by directly 
            // entering the corresponding URL
            // You need to create the dependency in project.json
            app.UseStaticFiles();
            
            // Enable MVC in the app, that allows us to define URLs that are mapped to Controllers that return Views that use Models to share the data
            // The "routes" parameter defines the routes in the form of a lambda function, which in this case is an Action<IRouteBuilder>
            app.UseMvc(routes => {
                routes.MapRoute(
                    // This is the default route, which is mapped to ???
                    name: "default",
                    // The controller is created in a folder Controllers/XXX, its name is DemoController.cs; the action name is mapped to a method of the controller; the id is an optional parameter
                    // XXX is a convention - Web for controllers that return Web pages, Api for controllers that return REST API responses
                    template: "{controller}/{action}/{id?}");
            });
        }

        // Entry point for the application.
        public static void Main(string[] args) => Microsoft.AspNet.Hosting.WebApplication.Run<Startup>(args);
    }
}
