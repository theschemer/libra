$(".btn").click(function(){
    $.ajax({
        url: "/blog/libra/17",
        success: function(data){
            var str = JSON.stringify(data);
            alert(str);
        }
    });
});